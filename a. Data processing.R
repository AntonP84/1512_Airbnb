
# loading data ------------------------------------------------------------

train     <- read.csv('data/raw/train_users.csv', stringsAsFactors = F)
test      <- read.csv('data/raw/test_users.csv', stringsAsFactors = F)
sessions  <- read.csv('data/raw/sessions.csv'  , stringsAsFactors = F)

var.target <- 'country_destination'
var.id     <- 'id'

test[, var.target] <- NA

df <- rbind(train, test)
df$date_first_booking <- NULL

rm(train, test)
gc()


# client info processing --------------------------------------------------
require(lubridate)

df <- within(df, {
          date_account_created          <- as.Date(date_account_created, '%Y-%m-%d')
          timestamp_first_active        <- strptime(as.character(timestamp_first_active), "%Y%m%d%H%M%S")
          
          signup_flow_factor           <- as.factor(signup_flow)
          
          b_Age_wrong         <- ifelse(age < 14 | age > 87, 1, 0)
          b_Age_105           <- ifelse(age == 105, 1, 0) # 0.5% выборки
          b_Age_2013          <- ifelse(age >= 2013, 1, 0)
          Age_adj             <- ifelse(age < 14 | (age > 90 & age < 1900) | age >= 2013, NA, ifelse(age >= 1900, 2014 - age, age))
          
          gender              <- ifelse(gender == '-unknown-', 'OTHER', gender)
          b_signup_app_mobile <- ifelse(signup_app %in% c('iOS', 'Android'), 1, 0) 
          
          b_signup_flow_typical <- ifelse((signup_method %in% c('google', 'weibo') & signup_flow == 12)
                                     | (signup_method %in% c('basic', 'facebook') & signup_flow == 0)
                                     , 1, 0)
          b_signup_app_typical <- ifelse((signup_app == 'Android' & first_device_type %in% c('Android Phone'))
                                   | (signup_app == 'iOS' & first_device_type %in% c('iPhone'))
                                   | (signup_app == 'Web' & first_device_type %in% c('Android Tablet', 'Desktop (Other)', 'iPad', 'Mac Desktop', 'Other/Unknown', 'SmartPhone (Other)', 'Windows Desktop'))
                                   , 1, 0)
          
          b_first_device_type_Desktop   <- ifelse(regexpr('Desktop', first_device_type) > 0, 1, 0)
          b_first_device_type_Android   <- ifelse(regexpr('Android', first_device_type) > 0, 1, 0)
          b_first_device_type_Phone     <- ifelse(regexpr('Phone', first_device_type) > 0, 1, 0)
          b_first_device_type_Apple     <- ifelse(first_device_type %in% c('iPad', 'iPhone', 'Mac Desktop'), 1, 0)
          b_first_device_type_iApple    <- ifelse(first_device_type %in% c('iPad', 'iPhone'), 1, 0)
          b_first_device_type_Tablet    <- ifelse(first_device_type %in% c('iPad', 'Android Tablet'), 1, 0)
          
          first_browser_adj <- ifelse(first_browser %in% c('Apple Mail', 'Mobile Safari'), 'Safari', first_browser)
          first_browser_adj <- ifelse(first_browser_adj %in% c('Chrome Mobile', 'Chromium'), 'Chrome', first_browser_adj)
          first_browser_adj <- ifelse(first_browser_adj %in% c('IE Mobile', 'Outlook 2007'), 'IE', first_browser_adj)
          first_browser_adj <- ifelse(first_browser_adj %in% c('Mobile Firefox', 'Mozilla'), 'Firefox', first_browser_adj)
          first_browser_adj <- ifelse(first_browser_adj %in% c('Opera Mini', 'Opera Mobile'), 'Opera', first_browser_adj)
          
          b_browser_nontypical <- ifelse(first_browser_adj == '-unknown-' 
                                         | (first_device_type == 'Android Phone' & first_browser_adj %in% c('Chrome', 'Android Browser'))
                                         | (first_device_type == 'Android Tablet' & first_browser_adj %in% c('IE', 'Android Browser'))
                                         | (first_device_type == 'iPad' & first_browser_adj == 'Safari')
                                         | (first_device_type == 'iPhone' & first_browser_adj == 'Safari')
                                         , 0, 1)
                    
})



# processing of client info date/time columns ----------------------------------

require(lubridate)
require(timeDate)


df <- within(df, {
          first_active_date             <- as.Date(timestamp_first_active)
          first_active_hour             <- hour(df$timestamp_first_active)
          first_active_minute           <- minute(df$timestamp_first_active)
          first_active_second           <- second(df$timestamp_first_active)
          timestamp_first_active        <- NULL
})

# old sessions are removed
df <- subset(df, first_active_date >= as.Date('2013-07-01', '%Y-%m-%d'))

vars <- names(df)
vars <- setdiff(vars, c(var.target, var.id))

class <- sapply(df[, vars], class)
class <- sapply(class, function(x) ifelse(sum(x %in% c("POSIXct", "POSIXt"))>0, 'Date', x))
table(class)

# date parts
for(i in vars[class == 'Date']){
          # df[, paste0(i, '_year_int')] <- year(df[, i])
          # df[, paste0(i, '_year')]     <- as.factor(year(df[, i]))
          # df[, paste0(i, '_quarter')]  <- as.factor(quarter(df[, i]))
          # df[, paste0(i, '_month_int')]<- month(df[, i])
          # df[, paste0(i, '_month')]    <- as.factor(month(df[, i]))
          # df[, paste0(i, '_week_int')] <- week(df[, i])
          # df[, paste0(i, '_week')]     <- as.factor(week(df[, i]))
          df[, paste0(i, '_wday')]     <- as.factor(wday(df[, i]))
          df[, paste0(i, '_is_weekend')] <- ifelse(wday(df[, i]) >= 6, 1, 0)
          df[, paste0(i, '_is_holiday')] <- isHoliday(as.timeDate(df[, i]), wday = 1:6) # ВСК и holidayNYSE
          df[, paste0(i, '_day_int')]  <- day(df[, i])
          # df[, paste0(i, '_day')]      <- as.factor(day(df[, i]))
          # df[, paste0(i, '_yday_int')] <- yday(df[, i])
          # df[, paste0(i, '_na')] <- is.na(df[, i])
          
          df[, paste0(i, '_same_season')]     <- ifelse(month(df[, i]) %in% c(7, 8, 9), 1, 0)
          df[, i] <- as.numeric(df[, i] )
}          

# pair-wise difference
for(i in vars[class == 'Date']){
          for(j in vars[class == 'Date']){
                    if(i != j) df[, paste('d', i, j, sep = '_')] <- df[, i] - df[, j]
          }}

# original dates are removed as test dataset covers the most recent period
for(i in vars[class == 'Date']) df[, i] <- NULL

rm(class, i, j, vars)



# one-hot encoding of categorical variables -------------------------------

vars  <- names(df)
vars  <- setdiff(vars, c(var.target, var.id))
class <- sapply(df[vars], class)
table(class)

nb.rare <- 0.001*nrow(df) # порог для редких уровней

for(i in vars[class %in% c("factor", "character")]){ 

          if(class(df[, i]) == 'factor') df[, i] <- as.character(df[, i])
                    
          # объединяем редкие уровни в один и записываем результат в x
          x <- table(df[i], useNA = 'ifany')
          x <- names(x[x<nb.rare]) # мелкие уровни
          x <- replace(df[, i], df[, i] %in% x, 'Z_RARE')
          x[is.na(x)] <- 'NA'

          # one-hot encoding
          x <- data.frame(x, stringsAsFactors = F)
          names(x) <- paste0(i, '_') # чтобы потом имя столбца = переменная_уровень
          
          df[, i] <- NULL
          df <- cbind(df, model.matrix(~.-1,x))
}

rm(class, i, nb.rare, vars, x)




# processing of sessions data ---------------------------------------------
require(dplyr)

sessions.aggr <- sessions %>%
          filter(user_id %in% df$id) %>%
          group_by(user_id) %>%
          summarise(
                    sessions.nb = n()
                    , type_view.pct   = sum(ifelse(action_type == 'view', 1, 0))/n()
                    , type_data.pct   = sum(ifelse(action_type == 'data', 1, 0))/n()
                    , type_click.pct  = sum(ifelse(action_type == 'click' , 1, 0))/n()
                    , type_null.pct   = sum(ifelse(action_type == '', 1, 0))/n()
                    , type_unk.pct    = sum(ifelse(action_type == '-unknown-', 1, 0))/n()
                    , type_submit.pct = sum(ifelse(action_type == 'submit', 1, 0))/n()
                    , type_message.pct= sum(ifelse(action_type == 'message_post', 1, 0))/n()
                    , type_request.pct= sum(ifelse(action_type == 'booking_request' , 1, 0))/n()
                    , type_active.pct = sum(ifelse(action_type %in% c('booking_request', 'booking_response', 'message_post', 'modify', 'partner_callback', 'submit'), 1, 0))/n()
                    , type_passive.pct= sum(ifelse(action_type %in% c('click', 'data', 'view'), 1, 0))/n()
                    
                    , action_plans.pct= sum(ifelse(action == 'travel_plans_current', 1, 0))/n()
                    , action_book.pct= sum(ifelse(action == 'book', 1, 0))/n()
                    , action_apply_coupon.pct= sum(ifelse(action == 'apply_coupon_click', 1, 0))/n()
                    , action_ask_q.pct = sum(ifelse(action == 'ask_question', 1, 0))/n()
                    , action_respond.pct= sum(ifelse(action == 'respond', 1, 0))/n()
                    , action_apply_reservation.pct= sum(ifelse(action == 'apply_reservation', 1, 0))/n()
                    , action_requested.pct= sum(ifelse(action == 'requested', 1, 0))/n()
                    , action_update.pct= sum(ifelse(action == 'update', 1, 0))/n()
                    , action_search.pct= sum(ifelse(action == 'search', 1, 0))/n()
                    , action_social_connections.pct= sum(ifelse(action == 'social_connections', 1, 0))/n()
                    , action_complete_status.pct= sum(ifelse(action == 'complete_status', 1, 0))/n()
                    , action_pending.pct= sum(ifelse(action == 'pending', 1, 0))/n()
                    , action_travel_plans_current.pct= sum(ifelse(action == 'travel_plans_current', 1, 0))/n()
                    , action_reviews.pct= sum(ifelse(action == 'reviews', 1, 0))/n()
                    , action_active.pct= sum(ifelse(action == 'active', 1, 0))/n()
                    , action_manage_listing.pct= sum(ifelse(action == 'manage_listing', 1, 0))/n()
                    , action_identity.pct= sum(ifelse(action == 'identity', 1, 0))/n()
                    , action_verify.pct= sum(ifelse(action == 'verify', 1, 0))/n()
                    , action_personalize.pct= sum(ifelse(action == 'personalize', 1, 0))/n()
                    , action_translate.pct= sum(ifelse(grep('translate', action) > 0, 1, 0))/n()
                    , action_message_host.pct= sum(ifelse(grep('message_to_host', action) > 0, 1, 0))/n()
                    
                    , action_ndf_high.pct= sum(ifelse(action %in% c('host_summary', 'uptodate', 'manage_listing', 'phone_verification_modal', 'set_user', 'recommend'), 1, 0))/n()
                    , action_ndf_low.pct= sum(ifelse(action %in% c('message_to_host_focus', 'phone_verification_success', 'slideshow', 'message_to_host_change', 'agree_terms_check', '12', 'pending', 'change', 'complete_status', 'ajax_price_and_availability', 'itinerary', 'requested', 'receipt', 'guest_booked_elsewhere', 'email_itinerary_colorbox', 'add_guests'), 1, 0))/n()
                    , action_ndf_very_low.pct= sum(ifelse(action %in% c('receipt', 'guest_booked_elsewhere', 'email_itinerary_colorbox', 'add_guests'), 1, 0))/n()
                    
                    , detail_contact_host.pct= sum(ifelse(action_detail == 'contact_host', 1, 0))/n()
                    , detail_p5.pct= sum(ifelse(action_detail == 'p5', 1, 0))/n()
                    , detail_update_listing.pct= sum(ifelse(action_detail == 'update_listing', 1, 0))/n()
                    , detail_message_thread.pct= sum(ifelse(action_detail == 'message_thread', 1, 0))/n()
                    , detail_your_trips.pct= sum(ifelse(action_detail == 'your_trips', 1, 0))/n()
                    , detail_listing_reviews.pct= sum(ifelse(action_detail == 'listing_reviews', 1, 0))/n()
                    , detail_post_checkout_action.pct= sum(ifelse(action_detail == 'post_checkout_action', 1, 0))/n()
                    , detail_create_phone_numbers.pct= sum(ifelse(action_detail == 'create_phone_numbers', 1, 0))/n()
                    , detail_listing.pct= sum(ifelse(grep('list', action_detail) > 0, 1, 0))/n()
                    
                    , detail_ndf_low.pct= sum(ifelse(action_detail %in% c('at_checkpoint', 'message_to_host_focus', 'phone_verification_success', 'message_to_host_change', 'change_or_alter', 'alteration_field', 'guest_itinerary', 'pending', 'p5', 'post_checkout_action', 'guest_receipt'), 1, 0))/n()

                    , device_type.nb = n_distinct(device_type)
                    , device_iPhone.pct= sum(ifelse(device_type == 'iPhone', 1, 0))/n()
                    , device_Mac.pct= sum(ifelse(device_type == 'Mac Desktop', 1, 0))/n()
                    , device_AndroidApp.pct= sum(ifelse(device_type == 'Android App Unknown Phone/Tablet', 1, 0))/n()
                    , device_AndroidPhone.pct= sum(ifelse(device_type == 'Android Phone', 1, 0))/n()
                    , device_Windows.pct= sum(ifelse(device_type == 'Windows Desktop', 1, 0))/n()
                    , device_iPodtouch.pct= sum(ifelse(device_type == 'iPodtouch', 1, 0))/n()
                    , device_iPad.pct= sum(ifelse(device_type == 'iPad Tablet', 1, 0))/n()
                    , device_unk.pct= sum(ifelse(device_type == '-unknown-', 1, 0))/n()
                    
                    , secs_elapsed.min = min(secs_elapsed, na.rm = T)
                    , secs_elapsed.max = max(secs_elapsed, na.rm = T)
                    , secs_elapsed.med = median(secs_elapsed, na.rm = T)
                    , secs_elapsed.range = secs_elapsed.max - secs_elapsed.min
          ) %>%
          rename( id = user_id)

df <- left_join(df, sessions.aggr, by = 'id')



# splitting data into train/test dataframes -------------------------------
vars <- names(df)

train <- subset(df
                , is.na(df[var.target]) == F
                , select = setdiff(vars, var.id)
)

test <- subset(df
                , is.na(df[var.target]) == T
                , select = setdiff(vars, var.target)
)

save(train, file = 'data/train_10_02.RData')
save(test, file = 'data/test_10_02.RData')