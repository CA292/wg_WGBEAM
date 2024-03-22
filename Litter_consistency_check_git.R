########################################################################
#   Check litter consistency accross offshore beam trawl surveys 
#   WGBEAM Meeting 2022
########################################################################
# Install and load packages
library(icesDatras)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Set years for which you want to request data
years <- c(2019:2022)

LT_ALL <- c()

for (i in seq_along(years)) {
  test <- getLTassessment("BTS", years[i], 3) %>%
    select(Survey, Quarter, Year, Ship, Gear, Country, StNo, HaulNo, ShootLat, ShootLong, HaulLat, HaulLong,
           BottomDepth, Distance, LTREF, PARAM, LTSZC, UnitWgt, LT_Weight, UnitItem, LT_Items, LTSRC,
           TYPPL, LTPRP, Month, Day, TimeShot, HaulDur, StatRec, Depth, HaulVal) %>%
    mutate(LTSRC = as.character(LTSRC)) 
    
  LT_ALL <- bind_rows(test, LT_ALL)
  }


# Create Material parameter
LT_ALL <- LT_ALL %>%
  mutate("Material" = case_when(PARAM == "LT-TOT" ~ "NA", TRUE ~ substr(PARAM, 1,1)))

#Make stacked charts which material is collected per country
ggplot(LT_ALL, aes(fill=Material, x=Country)) + 
  labs(title = "Comparison of litter classes BTS Survey") +
  geom_bar(position="fill", stat="count") + scale_fill_discrete(labels = c("Plastic", "Metal", "Rubber", "Glass/ceramics", "Natural products", "Miscellaneous", "No litter collected")) +
  facet_wrap(~Year) + theme(legend.title=element_text(size=30), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20)) +
  ylab("Ratio")

#Make stacked charts which class of plastic/rubber is collected per country
Zoom <- LT_ALL %>%
  filter(Material == "A" | Material == "C")

ggplot(Zoom, aes(fill=PARAM, x=Country)) + 
  labs(title = "Comparison of litter classes Plastic & Rubber BTS Survey") +
  geom_bar(position="fill", stat="count") + scale_fill_discrete(name = "Class", labels = c("A1: Bottle", "A10: Strapping band", "A11: Crates/Containers", "A12: Diapers", "A13: Towels/Tampons", "A14: Other", "A2: Sheet", "A3: Bag", "A4: Caps/lids", "A5: Monofilament", "A6: Entangled monofilament", "A7: Synthetic rope", "A8: Fishing net", "A9: Cable Ties", 
         "C1: Boots", "C2: Balloons", "C3: Bobbins (fishing)", "C4: Tyre", "C5: Glove", "C6: Other")) +
  facet_wrap(~Year) + theme(legend.title=element_text(size=24), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20)) +
  ylab("Ratio")

#Monofilament (what are sizes). Should be longest length multiplied with thickness 
#(for fishing monofilaments: 1mm --> length should be longer then 2,5 m (B:F unlikely))
ggplot(LT_ALL[LT_ALL$PARAM == "A5",], aes(fill=LTSZC, x=Year)) + 
  geom_bar(position="fill", stat="count") + ylab("Ratio") + scale_fill_discrete(name = "Size Class") +
  facet_wrap(~Country) + theme(legend.title=element_text(size=24), legend.text=element_text(size=25), plot.title = element_text(hjust = 0.5, size = 30), axis.text = element_text( size = 18), axis.text.x = element_text( size = 18, ), axis.title = element_text( size = 24),strip.text = element_text(size = 20))


Litterlist <- data.frame(PARAM  = c("A", "A1", "A10","A11","A12", "A13", "A14", "A15", "A16", "A2","A3", "A4", "A5", "A6", "A7", "A8", "A9", "B", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "C","C1", "C2", "C3","C4", "C5", "C6", "D", "D1", "D2", "D3", "D4", "E", "E1", "E2", "E3", "E4", "E5", "F", "F1", "F2", "F3"),
             Description = c("Plastic","Plastic bottle","Plastic strapping band", "Plastic crates and containers","Plastic diapers", "Sanitary towel/tampon", "Other plastics","Medical masks", "Other fishing related plastics", "Plastic sheet", "Plastic bag", "Plastic caps/lids", "Plastic fishing line (monofilament)", "Plastic fishing line (entangled)", "Synthetic rope", "Plastic fishing net", "Plastic cable ties", "Metals", "Cans (food)", "Cans (beverage)", "Fishing related metal", "Metal drums", "Metal appliances","Metal car parts","Metal cables","Other metal", "Rubber","Boots", "Balloons", "Rubber bobbins (fishing)", "Tyre","Glove", "Other rubber", "Glass/Ceramics", "Jar", "Glass bottle", "Glass/ceramic piece","Other glass or ceramic", "Natural products", "Wood (processed)", "Rope","Paper/cardboard","Pallets","Other natural products","Miscellaneous", "Clothing/rags", "Shoes", "Other"), 
            LT_Type = c("Plastic","Plastic","Plastic", "Plastic", "Plastic" , "Plastic","Plastic", "Plastic","Plastic", "Plastic", "Plastic", "Plastic","Plastic", "Plastic","Plastic","Plastic", "Plastic", "Metal"  ,"Metal",  "Metal"  ,"Metal", "Metal"  ,"Metal", "Metal"  ,"Metal", "Metal"  ,"Rubber", "Rubber" ,"Rubber", "Rubber" ,"Rubber", "Rubber" ,"Rubber", "Glass"  ,"Glass" , "Glass"  ,"Glass" , "Glass"  ,"Natural Products", "Natural Products", "Natural Products", "Natural Products", "Natural Products", "Natural Products", "Miscellaneous","Miscellaneous",    "Miscellaneous",  "Miscellaneous"))


LT_ALL2 <- left_join(LT_ALL, Litterlist)


LT_ALL2$LT_Weight <- ifelse(LT_ALL2$LT_Weight == -9, 0, LT_ALL2$LT_Weight)


LT_ALL2$LT_Wgt <- ifelse(LT_ALL2$UnitWgt == "kg/haul",  LT_ALL2$LT_Weight*1000, LT_ALL2$LT_Weight)


LT_ALL2$Length <- ifelse(LT_ALL2$LTSZC == "A",  "<25cm²", ifelse(LT_ALL2$LTSZC == "B",  "25-100cm²",  ifelse(LT_ALL2$LTSZC == "C",  "100-400cm²", ifelse(LT_ALL2$LTSZC == "D",  "400-2500cm²",  ifelse(LT_ALL2$LTSZC == "E",  "25cm²-1m²", ifelse(LT_ALL2$LTSZC == "D",  ">1m²",  ifelse(LT_ALL2$LTSZC == "9",  "1-2.5cm",ifelse(LT_ALL2$LTSZC == "10",  "2.5-4.9cm", ifelse(LT_ALL2$LTSZC == "11",  "5-9.9cm", ifelse(LT_ALL2$LTSZC == "12",  "10-14.9cm", ifelse(LT_ALL2$LTSZC == "13",  "15-49.9cm", ifelse(LT_ALL2$LTSZC == "14",  "50-99.9cm", ifelse(LT_ALL2$LTSZC == "15",  "100-199.9cm",ifelse(LT_ALL2$LTSZC == "16",  ">=200cm",NA)))))))))))))) # Used for looking at length distribution but found this different method used complicated so removed from analysis 


LitWgt <- LT_ALL2 |> dplyr::group_by(Survey, Quarter, Year, ShootLat, ShootLong, LT_Type) |> dplyr::summarise(LT_Wgt = sum(LT_Wgt), NoItem = sum(LT_Items)) |> ungroup() |> dplyr::filter(!LT_Type == "Total Litter") |> dplyr::filter(LT_Wgt > 0)

OverViewLit <- LT_ALL2 |> dplyr::group_by(Year,LT_Type, Description) |> dplyr::summarise(LT_Wgt = sum(LT_Wgt), Count = sum(LT_Items)) |> ungroup()


rangex <- c(min(LitWgt$ShootLong,na.rm=T)-1,max(LitWgt$ShootLong,na.rm=T)+1)
rangey <- c(min(LitWgt$ShootLat,na.rm=T)-1,max(LitWgt$ShootLat,na.rm=T)+1)


Chart2 <- ggplot()+
  ggtitle(paste0("BTS Survey 2022","\n", "Litter", sep=" "))+
  labs(subtitle =  "Grouped by Type")+
  geom_point(data = LitWgt[LitWgt$Year== 2022,],  aes(x =ShootLong, y = ShootLat, size = LT_Wgt, colour= LT_Type), stat = "identity", alpha = 0.5) +
  #geom_text(data=LitWgt, mapping = aes(x=ShootLong, y=ShootLat, label=TotalEgg), color = "black", size=3 , angle=0, fontface="bold" )+
  scale_size_continuous(range = c(.0001, 9), name="Weight (g/haul)")+
  borders("world", xlim = rangex, ylim = rangey, fill = "grey", colour = "light grey") +
  coord_quickmap(xlim = rangex, ylim = rangey) +
  facet_wrap(~LT_Type)+
  theme(plot.title = element_text(face="bold"), panel.grid.minor = element_line(colour = "NA") )+
  xlab("Longitude") +
  ylab("Latitude") 
Chart2

ggsave(filename = paste("LitterWgt2022.png",sep = ""), width = 30, height = 20, units = "cm")


Chart2 <- ggplot()+
  ggtitle(paste0("BTS Survey 2018-2022","\n", "Litter", sep=" "))+
  labs(subtitle =  "Grouped by Type")+
  geom_point(data = LitWgt,  aes(x =ShootLong, y = ShootLat, size = LT_Wgt, colour= LT_Type), stat = "identity", alpha = 0.5) +
  #geom_text(data=LitWgt, mapping = aes(x=ShootLong, y=ShootLat, label=TotalEgg), color = "black", size=3 , angle=0, fontface="bold" )+
  scale_size_continuous(range = c(.0001, 9), name="Weight (g/haul)")+
  borders("world", xlim = rangex, ylim = rangey, fill = "grey", colour = "light grey") +
  coord_quickmap(xlim = rangex, ylim = rangey) +
  facet_wrap(~Year)+
  theme(plot.title = element_text(face="bold"), panel.grid.minor = element_line(colour = "NA") )+
  xlab("Longitude") +
  ylab("Latitude") 
Chart2

ggsave(filename = paste("LitterWgtYr.png",sep = ""), width = 30, height = 20, units = "cm")

Chart2 <- ggplot()+
  ggtitle(paste0("BTS Survey 2018-2022","\n", "Litter", sep=" "))+
  labs(subtitle =  "Grouped by Type")+
  geom_point(data = LitWgt,  aes(x =ShootLong, y = ShootLat, size = NoItem, colour= factor(Year)), stat = "identity", alpha = 0.5) +
  #geom_text(data=LitWgt, mapping = aes(x=ShootLong, y=ShootLat, label=TotalEgg), color = "black", size=3 , angle=0, fontface="bold" )+
  scale_fill_brewer(palette="Set1", name="Year")+
  scale_size_continuous(range = c(.0001, 9), name="No. of Items")+
  borders("world", xlim = rangex, ylim = rangey, fill = "grey", colour = "light grey") +
  coord_quickmap(xlim = rangex, ylim = rangey) +
  facet_wrap(~Grouping)+
  theme(plot.title = element_text(face="bold"), panel.grid.minor = element_line(colour = "NA") )+
  xlab("Longitude") +
  ylab("Latitude") 
Chart2

ggsave(filename = paste("LitterCountYr.png",sep = ""), width = 30, height = 20, units = "cm")




LitWgt2 <- LT_ALL2 |> dplyr::group_by(Survey, Country, Year, ShootLat, ShootLong, Grouping) |> dplyr::summarise(LT_Wgt = sum(LT_Wgt), NoItem = sum(LT_Items)) |> ungroup() |> dplyr::filter(LT_Wgt ==0 & NoItem ==0)


Chart2 <- ggplot()+
  ggtitle(paste0("BTS Survey 2018-2022","\n", "DATRAS Litter Assessment Output", sep=" "))+
  labs(subtitle =  "No Litter Caught")+
  geom_point(data = LitWgt2,  aes(x =ShootLong, y = ShootLat, shape = Country), size = 1, colour = "dark green", stat = "identity", alpha = 0.5) +
  #geom_text(data=LitWgt, mapping = aes(x=ShootLong, y=ShootLat, label=TotalEgg), color = "black", size=3 , angle=0, fontface="bold" )+
  scale_fill_brewer(palette="Set1", name="Year")+
  scale_size_continuous(range = c(.0001, 9), name="No. of Items")+
  borders("world", xlim = rangex, ylim = rangey, fill = "grey", colour = "light grey") +
  coord_quickmap(xlim = rangex, ylim = rangey) +
  facet_wrap(~Year)+
  theme(plot.title = element_text(face="bold"), panel.grid.minor = element_line(colour = "NA") )+
  xlab("Longitude") +
  ylab("Latitude") 
Chart2

ggsave(filename = paste("NoLitter.png",sep = ""), width = 30, height = 20, units = "cm")










