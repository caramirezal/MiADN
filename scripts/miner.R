
## Extract power/endurance table data of page 13
power_endurance <- powerEnduranceMiner("/home/carlos/data/miadn/genotyping/")
write.csv(power_endurance,"../data/power_endurance.csv",row.names = FALSE)

## Extract recovery table data of page 13
data <- recoveryMiner("/home/carlos/data/miadn/genotyping/")
write.csv(data,"../data/recovery.csv",row.names = FALSE)

## Extract power table from page 6
power <- powerMiner("/home/carlos/data/miadn/genotyping/")
write.csv(power,"../data/power.csv",row.names = FALSE)

## Extract endurnace from page 6
endurance <- enduranceMiner("/home/carlos/data/miadn/genotyping/")
write.csv(endurance)
