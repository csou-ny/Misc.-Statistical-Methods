libname menustat "C:\Users\KoKo\Desktop\MenuStat";
proc import out= menustat.datlong2
			datafile="C:\Users\KoKo\Desktop\MenuStat\Annual_Data.csv"
			dbms=csv ;
			getnames= yes;
run;
