
# Load data ---------------------------------------------------------------

dat_db <- data_read_db(vsn = setting_get_version("data_version"))
dat_db_msr <- data_read_db_msr(vsn = setting_get_version("data_version"))
dat_station <- data_read_station(dtype = "tidy", vsn = setting_get_version("data_version"))
