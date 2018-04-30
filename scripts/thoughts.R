# if you get a spreadsheet of data
# and it contains wide-format columns
# is it possible to say:
# "this is a measurement, and its key is the column title"
# "and this is an attribute"
# and write a schema-tizer that will clean it up
# making it flexible enough to handle new additions, 
# provided that they're described as either measurements or attributes

# ...so, input data of rectangular shape, but untidy
# untidy_columns <- horrible delimited strings of the wide-attributes + measurement type
# tidy_attributes <- existing tidy-attribute columns
# apply untidy_columns in place of existing untidy ones
# measurements <- 'measurement_type' + wide-value attributes
# ...horribly delimited as needed
# gather(key = 'keys', value = 'values', -attributes)
# separate(keys, measurements, delimiter)
# spread(measurement_type, values)

# it's gathering to (multi-)key, value pairs
# at the existing tidy-attribute grain
# splitting keys as needed
# and spreading so each measure type has its own column

# it's like, pressing steel flat, and then folding it to desired thickness.
# the steel doesn't really change (ok, it does, but let's not talk about it)
# but now it's in the shape you want.


# You have a spreadsheet of data. Luckily, it's rectangular (missing values are ok).
# It is partially tidy: each date has a row, but the three regions are spread across columns.
# To make matters worse, the spreadsheet tracks spend, revenue, and sales reps.
# And none of the columns are in order: they were added on as they developed.
# So regions A and B have spend and sales reps, then both have revenue, then region C was added.
# So you have a date column, then three columns for each region, for a total of 10 columns.
# What do?
# First: create a list of the tidy columns. Here, that's only date.
tidy_columns <- c('date')
# Next: create a vector of the untidy columns.
# This shows: what are the measurement types and the full attribute values of the untidy columns?
# Here, they'd be one of the regions and one of the three values.
untidy_columns <- c('A.spend', 'A.reps', 'B.spend', 
                    'B.reps', 'A.revenue', 'B.revenue', 
                    'C.spend', 'C.revenue', 'C.reps')

# Now, we can press the data.
# By recording which columns are already tidy, we can leave them alone.
# And by recording the untidy columns in "attribute.measurement" format,
# we can treat them as keys, and do a very straightforward gather() operation.
# Then, because we delimited the untidy columns with '.', we can separate them:
# meaning we're going to get multiple tidy attribute columns, and one column
# showing what kind of measurement it is.
# The "kinds of measurements" belong in their own columns, 
# and now we can spread() the measurements to all of the tidy attributes at once


colnames(spreadsheet_df) <- c(tidy_columns, untidy_columns)


spreadsheet_df %>% 
    `colnames<-`(c(tidy_columns, untidy_columns)) %>% 
    gather(key = 'keys',
           value = 'values',
           -tidy_columns) %>% 
    separate(values, c('region', 'measurement_types'), '.') %>% 
    spread(measurement_types, values)

# Would like to investigate excelgesis also. 
# The right kinds of handling might ease metadata extraction.