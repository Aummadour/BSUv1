# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----


library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")




# 1.0 Selecting Columns with select() ----

# Basic Select

bike_orderlines_tbl %>%
    select(order_date, order_id, order_line)


bike_orderlines_tbl %>%
    select(starts_with("order_"))

#Reduce columns

bike_orderlines_tbl %>%
    select(order_date, total_price, category_1, category_2)

# Rearrange columns
    
bike_orderlines_tbl %>%
    select(bikeshop_name:state, everything())


# Select Helpers
# starts_with() ends_with() contains() matches() everything()


bike_orderlines_tbl %>%
    select(contains("price"))
    

    #pull()

# This select() gives you an error when you pipe the data into mean()
# because mean() only works with vectorized data, not tibble

# bike_orderlines_tbl %>%
#    select(total_price) %>%
#    mean()


# pull will make it work

bike_orderlines_tbl %>%
    pull(total_price) %>%
    mean()


bike_orderlines_tbl %>%
    pull(model)


# select_if

#using this to pull character data

bike_orderlines_tbl %>%
select_if(is.character)


bike_orderlines_tbl %>%
    select_if(is.numeric)


bike_orderlines_tbl %>%
select_if(~ !is.numeric(.))
# 2.0 Arranging with arrange() and desc() ----

bikes_tbl %>%
    select(model, price) %>%
    arrange(desc(price))  %>%  view()
?across()


# 3.0 Filtering Rows with filter() ----

bikes_tbl %>%
    select(model, price) %>%
    filter(price > mean(price))


# 3.1 filter(): formula filtering ----

bikes_tbl %>%
    select(model, price) %>%
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price))


bikes_tbl %>%
    select(model, price) %>%
    filter(price > 6000,
            model %>% str_detect("Supersix"))

# Filtering one or more conditions exactly using == and %in%
bike_orderlines_tbl %>%
    filter(category_2 %in% c("Over Mountain", "Trail", "Endurance Road"))

bike_orderlines_tbl %>%
    filter(category_2 == "Over Mountain")


bike_orderlines_tbl %>%
    filter(category_2 != "Over Mountain")

bike_orderlines_tbl %>%
    filter(!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))

# 3.2 slice(): filtering with row number(s) ----

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(price) %>%
    slice(1:5)

bikes_tbl %>% 
    arrange(desc(price)) %>%
    slice((nrow(.)-4):nrow(.))

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(93:97)


# 3.3 distinct():  Unique values

bike_orderlines_tbl %>%
    distinct(category_1)

bike_orderlines_tbl %>%
    distinct(category_1, category_2)

bike_orderlines_tbl %>%
    distinct(bikeshop_name)

# 4.0 Adding Columns with mutate() ----

bike_order_line_prices <- bike_orderlines_tbl %>%
    select(order_date, model, quantity, price) %>%
    mutate(total_price = quantity * price)


# Overwrite Column 
bike_order_line_prices %>%
    mutate(total_price = log(total_price))

# Transformations
bike_order_line_prices %>%
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_sqrt = total_price^0.5)

# Adding Flag
bike_order_line_prices %>%
    mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>%
    filter(is_supersix)

# Binning and if-then statements
    
    # Binning with ntile()
bike_order_line_prices %>%
    mutate(total_price_binned = ntile(total_price, 4))


    # if-then statements inside of mutate()
        # case_when - more flexible binning
bike_order_line_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High",
        total_price > quantile(total_price, 0.25) ~ "Medium",
        TRUE ~ "Low"
    ))
    
bike_order_line_prices %>%
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))


# 5.0 Grouping & Summarizing with group_by() and summarize() ----


    #Basics of summarize() and group_by()
bike_orderlines_tbl %>%
    summarise(
        revenue = sum(total_price)
    )


bike_orderlines_tbl %>%
    group_by(category_1) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup()

bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(revenue))

bike_orderlines_tbl %>%
    group_by(category_1, category_2, frame_material) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(revenue))


    # Summary Functions
bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(
        count = n(),
        avg = mean(total_price),
        med = median(total_price),
        sd = sd(total_price),
        min = min(total_price),
        max = max(total_price)
    ) %>% 
    ungroup() %>%
    arrange(desc(count))


    # summarize_all() - detect missing values
bike_order_line_missing <- bike_orderlines_tbl %>%
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))


bike_order_line_missing %>%
    summarise_all(~ sum(is.na(.)))

bike_order_line_missing %>%
    summarise_all(~ sum(is.na(.)) / length(.) * 100)

    # Options for handling missing values
        # filter()  - Remove
             # Tidyr Package also has:
                #       drop_na() - Remove (filter shortcut)
                #       fill() - Replace up/down
                #       replace_na() - Replace by specifying
            #       impute() - programmatically replace (Recipes package)
bike_order_line_missing %>%
    filter(!is.na(total_price))

# 6.0 Renaming columns with rename() and set_names() ----





# 6.1 rename: One column at a time ----
bike_shop_revenue_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, category_1, total_price) %>%
    
    group_by(bikeshop_name, category_1) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(sales))

bike_shop_revenue_tbl %>%
    rename(
        `Bikeshop Name` = bikeshop_name,
        `Primary Category` = category_1,
        `Sales` = sales
    )

# 6.2 set_names: All columns at once ---

bike_shop_revenue_tbl %>%
    set_names(c("Bikeshop Name", "Primary Category", "Sales"))

bike_shop_revenue_tbl %>%
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----


# 7.1 spread(): Long to Wide ----
bike_shop_formatted_tbl <- bike_shop_revenue_tbl %>%
    spread(key = category_1, value = sales) %>%
    arrange(desc(Mountain)) %>%
    rename(`Bikeshop Name` = bikeshop_name) %>%
    
    mutate(
        Mountain = scales::dollar(Mountain),
        Road = scales::dollar(Road)
    )

# 7.2 gather(): Wide to Long ----

bike_shop_formatted_tbl %>%
    gather(key = "catergory_1", value = "sales", Mountain, Road)
        # or
bike_shop_formatted_tbl %>%
    gather(key = "catergory_1", value = "sales", -`Bikeshop Name`) %>%
    
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>%
    arrange(desc(sales))


# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----

orderlines_tbl %>%
    left_join(y = bikes_tbl, by = c("product.id" = "bike.id"))
    


# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----
bike_orderlines_tbl %>%
    select(-contains("order")) %>%
    
    bind_cols(
        bike_orderlines_tbl %>% select(order_id)
    )


# 9.2 bind_rows() ----

train_tbl <- bike_orderlines_tbl %>%
    slice(1:(nrow(.)/2))




test_tbl <- bike_orderlines_tbl %>%
    slice((nrow(.)/2 + 1):nrow(.))


train_tbl %>%
    bind_rows(test_tbl)


# 10.0  Manipulating Text in Columns ----
# separate() and unite()

bike_orderlines_tbl %>%
    select(order_date) %>%
    mutate(order_date = as.character(order_date)) %>%


    #separate
    separate(col = order_date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
    
    mutate(
        year  = as.numeric(year),
        month = as.numeric(month),
        day   = as.numeric(day)
    )  %>%

    #unite
        unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
    
mutate(order_date_united = as.Date(order_date_united))
