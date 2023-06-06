#' Final energy demand for feedstocks (non-energy use)
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues, Michaja Pehl, Simón Moreno
#'
#' @seealso [`calcOutput()`][madrat::calcOutput].
#' @md
#'

calcnonEnergyIndFE <- function() {

  # ---- read region mapping ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional') %>%
    as_tibble() %>%
    select(iso3c = CountryCode, region = RegionCode, H12 = missingH12) %>%
    mutate(H12 = ifelse('rest' == H12, region, H12))

  # ---- new region mapping for aggregation ----
  region_aggregation_mapping <- bind_rows(
    region_mapping %>%
      select(iso3c, region),

    region_mapping %>%
      select(iso3c, region = H12),

    region_mapping %>%
      select(iso3c) %>%
      mutate(region = 'World')
  ) %>%
    distinct(iso3c, region)

  # read in data----
  # ---- read IEA Energy Balance data ----
  # and convert to usable data frame
  IEA_EB <- readSource('IEA', 'EnergyBalances') %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(iso3c = Region, product = Data1, flow = Data2, year = Year,
           value = Value) %>%
    character.data.frame() %>%
    mutate(year = as.integer(year),
           value = value  * 4.1868e-05)


  # ---- get population data ----
  population <- calcOutput(type = 'PopulationPast', aggregate = FALSE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(iso3c = Region, year = Year, population = Value) %>%
    mutate(year = as.integer(as.character(year)),
           population = population * 1e6)

  # ---- read UNIDO Indstat data ----
  UNIDO_INDSTAT <- read_csv(file = paste0(getConfig('sourcefolder'),
                                          '/UNIDO/INDSTAT2/INDSTAT2_2017_ISIC_Rev_3.csv'),
                            col_names = c('ctable', 'country', 'year', 'isic', 'isiccomb',
                                          'value', 'utable', 'lastupdated', 'unit'),
                            col_types = 'ciiccdc-ic',
                            na = '...') %>%
    filter(isic %in% c('24', '25'), # select Chemicals and chemical products,
           # and Rubber and plastics products
           '25A' != isiccomb,       # exclude Non-metallic mineral products
           '20' == ctable,          # select Value Added
           !is.na(value)) %>%
    group_by(country, year) %>%
    summarise(value.added = sum(value), .groups = 'drop') %>%
    # replace M49 country codes with ISO-3166 alpha-3
    inner_join(
      bind_rows(
        countrycode::codelist %>%
          select('un', 'iso3c'),

        tribble(~un,   ~iso3c,
                158,   'TWN',   # Republic of China (aka Taiwan)
                200,   'CSK',   # Czechoslovakia
                230,   'Exx',   # Ethiopia and Eritrea
                # 231 is ETH
                # 232 is ERI
                278,   'GDR',   # German Democratic Republic
                280,   'BRG',   # Federal Republic of Germany (pre 1990)
                530,   'ANT',   # Netherlands Antilles
                590,   'PAN',   # Panama
                736,   'SDN',   # Sudan (including South Sudan)
                810,   'SUN',   # Soviet Union
                890,   'YUG',   # Yugoslavia
                891,   'SCG'    # Serbia and Montenegro
        )
      ),

      c('country' = 'un')
    ) %>%
    select(-country)

  # ---- replace historic countries with their current successors ----
  # read list of country replacements from madbrat package
  country_replacements <- bind_rows(
    read_delim(
      file = system.file('extdata', 'ISOhistorical.csv', package = 'madrat'),
      delim = ';',
      col_names = c('iso3c.from', 'iso3c.to', 'last.year'),
      col_types = 'ccc',
      skip = 1) %>%
      mutate(last.year = as.integer(sub('^y', '', last.year))),

    # extend by Ethiopia and Eritrea
    tibble(iso3c.from = 'Exx',
           iso3c.to   = c('ETH', 'ERI'),
           last.year  = 1992)
  )

  UNIDO_INDSTAT <- bind_rows(
    UNIDO_INDSTAT %>%
      filter(!iso3c %in% unique(country_replacements$iso3c.from)),

    inner_join(
      UNIDO_INDSTAT,

      # use population figures for weights
      inner_join(
        country_replacements,

        population %>%
          filter(iso3c %in% unique(country_replacements$iso3c.to)),

        c('iso3c.to' = 'iso3c')
      ) %>%
        filter(year <= last.year) %>%
        select(-last.year) %>%
        group_by(iso3c.from, year) %>%
        mutate(share = population / sum(population)) %>%
        ungroup() %>%
        select(-population),

      c('iso3c' = 'iso3c.from', 'year')
    ) %>%
      mutate(value.added = value.added * share) %>%
      select(-iso3c, iso3c = iso3c.to, -share)
  )
  # read in NECHEM data----
  data_NE.carbon <- IEA_EB %>%
    # select NECHEM data
    filter('TOTAL' != product, 'NECHEM' == flow) %>%
    # join NECHEM data by product with output mapping to separate
    # solids/liquids/gases
    left_join(
      toolGetMapping('structuremappingIO_outputs.csv', 'sectoral') %>%
        as_tibble() %>%
        select(product = iea_product, flow = iea_flows,
               pf.in = REMINDitems_in, pf.out = REMINDitems_out) %>%
        filter(grepl('^fe..i', pf.out)) %>%
        distinct(product, pf.out) %>%
        mutate(pf.out = sub('^(fe..i).*', '\\1', pf.out)) %>%
        inner_join(
          tribble(~pf.out,   ~enty,
                  'fesoi',   'solids',
                  'fehoi',   'liquids',
                  'fegai',   'gases'),

          'pf.out') %>%
        # modify Blast Furnace/Coke Oven/Gas Works outputs to exclude the
        # 'solids' cludge and make them only produce gases
        filter(!(product %in% c('BLFURGS', 'COKEOVGS', 'GASWKSGS', 'OGASES')
                 & 'solids' == enty)) %>%
        distinct() %>%
        group_by(product) %>%
        mutate(count = n()) %>%
        ungroup() %>%
        verify(1 == count,
               description = paste('verify structuremappingIO_outputs.csv',
                                   'to give 1-to-1 mapping of NECHEM',
                                   'products to REMIND enty')) %>%
        select(-count, -pf.out),

      'product'
    ) %>%
    # join with emission factors to get MtC/EJ
    inner_join(
      # GtC/ZJ * (0.001 ZJ/EJ) * 1000 Mt/Gt = MtC/EJ
      tribble(~enty,   ~emission.factor,
              # emission factors taken from
              # c463c6a ./core/input/generisdata_emi.prn
              'solids',    26.1,
              'liquids',   20,
              'gases',     15.3),

      'enty'
    ) %>%
    # calculate non-energy carbon use
    group_by(iso3c, year) %>%
    # EJ * MtC/EJ = MtC
    summarise(NE.carbon = sum(value * emission.factor), .groups = 'drop')

  # regression of added value and carbon content in feedstocks per capita----
  regression_data <- population %>%
    inner_join(UNIDO_INDSTAT, c('iso3c', 'year')) %>%
    inner_join(data_NE.carbon, c('iso3c', 'year')) %>%
    inner_join(region_aggregation_mapping,  'iso3c') %>%
    pivot_longer(c('population', 'value.added', 'NE.carbon')) %>%
    group_by(region, year, name) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    pivot_wider() %>%
    mutate(not_censored = ifelse('CHA' == region & !year %in% 1980:2007,
                                 FALSE, TRUE)) %>%
    mutate(value.added.pC = value.added / population,
           # EJ * 1e9 GJ/EJ = GJ
           NE.carbon.pC   = NE.carbon   / population * 1e9) %>%
    select(-value.added, -NE.carbon)

  regression_parameters <- regression_data %>%
    nest(data = -region) %>%
    mutate(fit = map(data, ~lm(formula = NE.carbon.pC ~ log(value.added.pC),
                               data = .x,
                               weights = population * as.integer(not_censored))),
           tidied = map(fit, tidy)) %>%
    unnest(tidied) %>%
    select(region, name = term, value = estimate) %>%
    pivot_wider() %>%
    rename(intercept = `(Intercept)`,
           slope     = `log(value.added.pC)`)

  # plots for testing----

  # for (r in list(c(sort(unique(region_mapping$H12)), 'World'),
  #                region_mapping %>%
  #                filter(region != H12) %>%
  #                pull(region) %>%
  #                unique() %>%
  #                sort()))
  # {
  #   browser()
  #   p <- ggplot() +
  #     geom_point(data = regression_data %>%
  #                  filter(region %in% r,
  #                         not_censored),
  #                mapping = aes(x = value.added.pC, y = NE.carbon.pC),
  #                colour = 'darkgrey') +
  #     geom_path(
  #       data = regression_data %>%
  #         filter(region %in% r,
  #                not_censored) %>%
  #         group_by(region) %>%
  #         summarise(min = min(value.added.pC),
  #                   max = max(value.added.pC),
  #                   .groups = 'keep') %>%
  #         mutate(value.added.pC = NA_real_) %>%
  #         complete(nesting(region),
  #                  value.added.pC = seq(min, max, length.out = 1000)) %>%
  #         ungroup() %>%
  #         select(-min, -max) %>%
  #         inner_join(regression_parameters, 'region') %>%
  #         mutate(NE.carbon.pC = intercept + slope * log(value.added.pC)),
  #       mapping = aes(x = value.added.pC, y = NE.carbon.pC)) +
  #     facet_wrap(~ region, scales = 'free') +
  #     scale_x_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     scale_y_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     labs(x = 'per-capita Chemicals Value Added [$]',
  #          y = 'per-capita Non-Energy Carbon Use in Chemicals [kgC]') +
  #     theme_minimal()
  #   plot(p)
  # }

  # projections----
  projection_data <- bind_rows(
    calcOutput(type = 'FEdemand', subtype = 'FE',
               aggregate = FALSE)[,,'ue_chemicals'] %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = Region, SSP = Data1, year = Year, value = Value) %>%
      character.data.frame() %>%
      mutate(year = as.integer(as.character(year)),
             SSP = sub('^.*_', '', SSP),
             name = 'chemicals.VA',
             # $tn * 1e12 $/$tn = $
             value = value * 1e12),

    calcOutput(type = 'Population', aggregate = FALSE) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = Region, SSP = Data1, year = Year, value = Value) %>%
      character.data.frame() %>%
      mutate(year = as.integer(as.character(year)),
             SSP = sub('^.*_', '', SSP),
             name = 'population',
             # million * 1e6 /million = 1
             value = value * 1e6)
  ) %>%
    left_join(region_aggregation_mapping, 'iso3c') %>%
    group_by(region, SSP, name, year) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    pivot_wider() %>%
    filter(!is.na(chemicals.VA), !is.na(population)) %>%
    left_join(regression_parameters, 'region') %>%
    mutate(
      NE.carbon.pC = (intercept + slope * log(chemicals.VA / population))) %>%
    select(-intercept, -slope)

  projection_data <- inner_join(
    projection_data %>%
      filter('World' != region),

    projection_data %>%
      filter('World' == region) %>%
      select(-region, -population, -chemicals.VA,
             NE.carbon.pC.world = NE.carbon.pC),

    c('year', 'SSP')
  ) %>%
    mutate(
      l = 1 - pmin(1, pmax(0, (year - 2020) / (2100 - 2020))),
      NE.carbon.pC = NE.carbon.pC * l + NE.carbon.pC.world * (1 - l),
      NE.carbon = NE.carbon.pC * population * 1e-9) %>%
    select(-l, -NE.carbon.pC.world, -NE.carbon.pC)

  # more plots----
  # for (r in list(c(sort(unique(region_mapping$H12)), 'World'),
  #                region_mapping %>%
  #                filter(region != H12) %>%
  #                pull(region) %>%
  #                unique() %>%
  #                sort()))
  # {
  #   p <- ggplot() +
  #     geom_point(data = regression_data %>%
  #                  filter(region %in% r,
  #                         not_censored),
  #                mapping = aes(x = value.added.pC, y = NE.carbon.pC),
  #                colour = 'darkgrey') +
  #     geom_path(
  #       data = regression_data %>%
  #         filter(region %in% r,
  #                not_censored) %>%
  #         group_by(region) %>%
  #         summarise(min = min(value.added.pC),
  #                   max = max(value.added.pC),
  #                   .groups = 'keep') %>%
  #         mutate(value.added.pC = NA_real_) %>%
  #         complete(nesting(region),
  #                  value.added.pC = seq(min, max, length.out = 1000)) %>%
  #         ungroup() %>%
  #         select(-min, -max) %>%
  #         inner_join(regression_parameters, 'region') %>%
  #         mutate(NE.carbon.pC = intercept + slope * log(value.added.pC)),
  #       mapping = aes(x = value.added.pC, y = NE.carbon.pC)) +
  #     geom_path(
  #       data = projection_data %>%
  #         filter(region %in% r),
  #       mapping = aes(x = chemicals.VA / population,
  #                     y = NE.carbon / population * 1e9,
  #                     colour = SSP)) +
  #     scale_colour_discrete(name = NULL) +
  #     facet_wrap(~ region, scales = 'free') +
  #     scale_x_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     scale_y_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     labs(x = 'per-capita Chemicals Value Added [$]',
  #          y = 'per-capita Non-Energy Carbon Use in Chemicals [kgC]') +
  #     theme_minimal()
  #   plot(p)
  # }
  #
  # for (r in list(c(sort(unique(region_mapping$H12)), 'World'),
  #                region_mapping %>%
  #                filter(region != H12) %>%
  #                pull(region) %>%
  #                unique() %>%
  #                sort()))
  # {
  #   p <- ggplot() +
  #     geom_point(data = regression_data %>%
  #                  filter(region %in% r,
  #                         not_censored),
  #                mapping = aes(x = value.added.pC * population * 1e-9,
  #                              y = NE.carbon.pC * population * 1e-9),
  #                colour = 'darkgrey') +
  #     geom_path(
  #       data = projection_data %>%
  #         filter(region %in% r),
  #       mapping = aes(x = chemicals.VA * 1e-9,
  #                     y = NE.carbon,
  #                     colour = SSP)) +
  #     scale_colour_discrete(name = NULL) +
  #     facet_wrap(~ region, scales = 'free') +
  #     scale_x_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     scale_y_continuous(limits = c(0, NA),
  #                        expand = expansion(mult = c(0, 0.05))) +
  #     labs(x = 'Chemicals Value Added [$bn]',
  #          y = 'Non-Energy Carbon Use in Chemicals [MtC]') +
  #     theme_minimal()
  #   plot(p)
  # }
  #
  # data_plot <- bind_rows(
  #   projection_data %>%
  #     filter(region %in% unique(region_mapping$H12)) %>%
  #     pivot_longer(c('population', 'chemicals.VA',
  #                    'NE.carbon')) %>%
  #     sum_total(region, name = 'World'),
  #
  #   projection_data %>%
  #     filter(!region %in% unique(region_mapping$H12)) %>%
  #     pivot_longer(c('population', 'chemicals.VA',
  #                    'NE.carbon'))
  # )
  #
  # for (r in list(c(sort(unique(region_mapping$H12)), 'World'),
  #                region_mapping %>%
  #                filter(region != H12) %>%
  #                pull(region) %>%
  #                unique() %>%
  #                sort()))
  #   for (n in unique(data_plot$name))
  #   {
  #     p <- ggplot() +
  #       geom_path(
  #         data = data_plot %>%
  #           filter(2100 >= year,
  #                  region %in% r,
  #                  n == name),
  #         mapping = aes(x = year, y = value, colour = SSP,
  #                       alpha = SSP)) +
  #       scale_colour_discrete(name = NULL) +
  #       scale_alpha_manual(values = c('SDP'  = 0.5,
  #                                     'SSP1' = 1,
  #                                     'SSP2' = 1,
  #                                     'SSP3' = 0.5,
  #                                     'SSP4' = 0.5,
  #                                     'SSP5' = 1),
  #                          guide = 'none') +
  #       facet_wrap(~ region, scales = 'free_y') +
  #       scale_y_continuous(limits = c(0, NA),
  #                          expand = expansion(mult = c(0, 0.05))) +
  #       labs(x = NULL, y = 'Index [2020 = 1]', subtitle = n) +
  #       theme_minimal()
  #     plot(p)
  #   }

  # calculate output----
  # c('* description: projections of chemicals non-energy use',
  #   '* unit: EJ',
  #   paste('* origin: separate script foo.Rmd at',
  #         'git@gitlab.pik-potsdam.de:REMIND/EDGE-Industry.git'),
  #   paste('* creation date: ', Sys.time())) %>%
  #   write_lines(file = 'pm_fe_nechem.cs4r',
  #               append = FALSE)

  # d_FE <- calcOutput(whatever generates pm_fe_demand.cs4r ) %>% etc...

  # d_FE_old <- read_csv(
  #   file = paste0('old/',
  #                 'pm_fe_demand.cs4r'),
  #   col_names = c('t', 'regi', 'SSP', 'pf', 'value'),
  #   col_types = 'icccd',
  #   comment = '*') %>%
  #   filter(pf %in% c('fesoi', 'fehoi', 'fegai'))


  d_FE_new <- calcOutput("FEdemand") %>%
    as.quitte() %>%
    select(c(-model,-variable,-unit)) %>%
    filter(item %in% c('fesoi', 'fehoi', 'fegai')) %>%
    rename(
      SSP = scenario,
      pf = item,
      regi = region,
      t = period
    )


  foo <- IEA_EB %>%
    # select NECHEM data
    filter(min(d_FE_new$t) <= year, 'TOTAL' != product, 'NECHEM' == flow,
           0 != value) %>%
    rename(period = year) %>%
    # join NECHEM data by product with output mapping to separate
    # solids/liquids/gases
    inner_join(
      toolGetMapping('structuremappingIO_outputs.csv', 'sectoral') %>%
        as_tibble() %>%
        select(product = iea_product, flow = iea_flows,
               pf.in = REMINDitems_in, pf.out = REMINDitems_out) %>%
        filter(grepl('^fe..i', pf.out)) %>%
        distinct(product, pf.out) %>%
        mutate(pf.out = sub('^(fe..i).*', '\\1', pf.out)) %>%
        # modify Blast Furnace/Coke Oven/Gas Works outputs to exclude the
        # 'solids' cludge and make them only produce gases
        filter(!(product %in% c('BLFURGS', 'COKEOVGS', 'GASWKSGS', 'OGASES')
                 & 'fesoi' == pf.out)) %>%
        distinct() %>%
        group_by(product) %>%
        mutate(count = n()) %>%
        ungroup() %>%
        verify(1 == count,
               description = paste('verify structuremappingIO_outputs.csv',
                                   'to give 1-to-1 mapping of NECHEM',
                                   'products to REMIND enty')) %>%
        filter(pf.out %in% c('fesoi', 'fehoi', 'fegai')) %>%
        select(product, pf = pf.out),

      'product'
    ) %>%
    left_join(
      region_mapping %>%
        select(-H12),

      'iso3c'
    ) %>%
    group_by(region, pf, period) %>%
    summarise(value = sum(value), .groups = 'drop')

  bind_rows(
    # historic data
    foo %>%
      filter(2015 > period) %>%
      # expand SSP scenarios
      mutate(SSP = first(projection_data$SSP)) %>%
      complete(nesting(region, pf, period, value),
               SSP = unique(projection_data$SSP)),

    # continue 2015 fesoi/fehoi/fegai shares with NE.carbon growth rate
    foo %>%
      filter(2015 == period) %>%
      select(-period) %>%
      left_join(
        projection_data %>%
          filter(2015 <= year) %>%
          select(-population, -chemicals.VA) %>%
          group_by(region, SSP) %>%
          mutate(factor = NE.carbon / first(NE.carbon)) %>%
          ungroup() %>%
          select(-NE.carbon),

        'region'
      ) %>%
      mutate(value = value * factor) %>%
      select(-factor) %>%
      # limit non-energy share in otherInd to 2015 levels
      left_join(
        d_FE_new %>%
          filter(2015 <= t) %>%
          mutate(SSP = sub('^gdp_', '', SSP)) %>%
          select(region = regi, SSP, pf, year = t, otherInd = value),

        c('region', 'SSP', 'pf', 'year')
      ) %>%
      arrange(pf, year) %>%
      group_by(region, SSP, year) %>%
      mutate(share = sum(value) / sum(otherInd)) %>%
      ungroup(year) %>%
      mutate(value = value / pmax(share, first(share)) * first(share)) %>%
      ungroup() %>%
      select(region, SSP, pf, period = year, value)
  )

  x <- as.magpie(foo)

  # %>%
  #   filter(period %in% unique(remind_timesteps$period)) %>%
  #   mutate(pf = paste0(pf, '_nechem')) %>%
  #   select(period, region, SSP, pf, value) %>%
  #   arrange(period, region, SSP, pf) %>%
  #   #this function must not write a file
  #   write_csv(file = 'pm_fe_nechem.cs4r',
  #             append = TRUE,
  #             col_names = FALSE)


#transform dataframe into magpie class

  # assign output----

#  x <- readSource("Calculate here!")

  return(list(x           = x,
              weight      = NULL,
              unit        = "EJ",
              description = "Final energy demand for feedstocks (non-energy use)"))
}
