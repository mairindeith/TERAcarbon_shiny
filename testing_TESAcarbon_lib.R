# testing the TESAcarbon package

library(TESAcarbon)

# Column names:
names(ICES)

#  [1] "activity.type"     "activity.name"     "origin"           
#  [4] "destination"       "origin.country"    "hotel.nights"     
#  [7] "meals"             "bustrain.distance" "car.distance"     
# [10] "car.sharing"       "flying"  

# > head(ICES)
#    activity.type                    activity.name     origin destination
# 1:        course Introduction to Stock Assessment     Bergen  Copenhagen
# 2:        course Introduction to Stock Assessment   Goteborg  Copenhagen
# 3:        course Introduction to Stock Assessment Copenhagen  Copenhagen
# 4:        course Introduction to Stock Assessment   Aberdeen  Copenhagen
# 5:        course Introduction to Stock Assessment  Lowestoft  Copenhagen
# 6:        course Introduction to Stock Assessment     Galway  Copenhagen
#    origin.country hotel.nights meals bustrain.distance car.distance car.sharing
# 1:         Norway            5    16                 0            0           0
# 2:         Sweden            5    16               300            0           0
# 3:        Denmark            5    16                 0            0           0
# 4:             UK            5    16                 0            0           0
# 5:             UK            5    16                 0            0           0
# 6:        Ireland            5    16                 0            0           0
#    flying
# 1:      1
# 2:      0
# 3:      0
# 4:      1
# 5:      1
# 6:      1

