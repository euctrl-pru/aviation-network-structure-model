# data sample preparation
weeks <- 26:31

# Create sample data
net2019 <- fread("df_2019.csv") %>%
  filter(week %in% weeks) %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2020 <- fread("df_2020.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2021 <- fread("df_2021.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2022 <- fread("df_2022.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

fwrite(net2019 ,file = "sample_2019.csv")
fwrite(net2020 ,file = "sample_2020.csv")
fwrite(net2021 ,file = "sample_2021.csv")
fwrite(net2022 ,file = "sample_2022.csv")
