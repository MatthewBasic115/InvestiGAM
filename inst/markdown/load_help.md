# Loading Data

InvestiGAM uses the vroom package to load its data. This supports various delimited files including comma separated values (CSVs).

Select the file you want to upload and it's delimiter. A comma (",") is selected by default so if you are uploading a CSV then you should be good to go immediately.

One issue you may face is with column data types. Although vroom will make a best guess at the data type, you will likely need to specify common data types such as factors. This is where the Column Types text field is useful. You can enter in plain text the column type for your variables.

The column type input uses the same syntax as readr with the following specifications (from vroom doco).

    col_logical() ‘l’, containing only T, F, TRUE, FALSE, 1 or 0.
    col_integer() ‘i’, integer values.
    col_big_integer() ‘I’, Big integer values. (64bit integers)
    col_double() ‘d’, floating point values.
    col_number() ‘n’, numbers containing the grouping_mark
    col_date(format = "") ‘D’: with the locale’s date_format.
    col_time(format = "") ‘t’: with the locale’s time_format.
    col_datetime(format = "") ‘T’: ISO8601 date times.
    col_factor(levels, ordered) ‘f’, a fixed set of values.
    col_character() ‘c’, everything else.
    col_skip() ’_, -’, don’t import this column.
    col_guess() ‘?’, parse using the “best” type based on the input.

You do not need to define every single column. If you only need to define the 'plant' column as a factor, then you can simply enter: plant="f" and all other columns will have their datatype guessed.

See the vroom doco at https://vroom.r-lib.org/articles/vroom.html#column-types for more information.
