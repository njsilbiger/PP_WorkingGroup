#Read in the data from google sheets ####
### Created by Nyssa Silbiger ####
### 2023-09-07 #######

## load libraries #####
library(googlesheets4)

## Read in the different data sheets ####
#Read google sheets data into R.

# NOTE: Some of these files are HUGE! Only read in what you need so your computer won't crash

# Benthic Cover data
BenthicCover <- read_sheet('https://docs.google.com/spreadsheets/d/1iA8rP_raCQ8NTPUGqAZ6LVeYza7eKqU4ep6yucpj1U0/edit?usp=sharing')

# CHN Turbinaria
CHN<-read_sheet("https://docs.google.com/spreadsheets/d/18BCnI9u5h4pOQ3n0RCT00TSRcqvBYsP7OQsmFmAGjac/edit?usp=sharing")

## Forereef currents and waves
waves<-read_sheet("https://docs.google.com/spreadsheets/d/1GxvdqxKAzcYguyfy9kAeizDZ4Zx3ilkTwQqYQQjkgIs/edit?usp=sharing")

## Fish data
fish<-read_sheet("https://docs.google.com/spreadsheets/d/156ipwCIqyhYW-XKDcbz5Q6CC_xE3X1er_urb2Dk-sbQ/edit?usp=sharing")


# Invert data
inverts<-read_sheet("https://docs.google.com/spreadsheets/d/1YempqAceHmRMUnOaUC_2Qmpw6Bap3BDAfxjMxp7s8Ws/edit?usp=sharing")

## Temperature LTER 1
Temp_LTER1<-read_sheet("https://docs.google.com/spreadsheets/d/1YJRyNAeo03xLoThBGX2-BtHpWqUpYXIarmYTsIhw5aI/edit?usp=sharing")

## Temperature LTER 2
Temp_LTER2<-read_sheet("https://docs.google.com/spreadsheets/d/1RdidcRsXZjPbLE9wIlbQ4i13G097MkVWzQn59HVLJZc/edit?usp=sharing")

## Primary production and calcification data 
PP<-read_sheet("https://docs.google.com/spreadsheets/d/1dl085D-DqLS5M1LfWZ60QpDHhI2_fPdhknwNtqxy6WM/edit?usp=sharing")

## Raw oxygen data -- this file is huge
PP_raw<-read_sheet("https://docs.google.com/spreadsheets/d/1buXothuiEjY0uVnDXyrYiHd7m34wr40m2hW3dI6KjGc/edit?usp=sharing")

