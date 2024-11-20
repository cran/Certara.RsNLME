# correct cols1.txt is built with DOSING CYCLE block

    {
      "type": "character",
      "attributes": {},
      "value": ["id(\"id\")", "time(\"time\")", "dose(Aa<-\"dose_Aa\")", "obs(CObs<-\"CObs\")", "table(file=\"posthoc.csv\", Ka, V, Cl, mode=keep)", "", "ss(\"ss1\", 10 dup / 5 inf(Aa) 5 dt)\naddl(\"addl1\", 5 dt 10 bolus(Aa))\n"]
    }

# correct cols1.txt is built with categorical covariate lables in MAP block

    {
      "type": "character",
      "attributes": {},
      "value": ["id(\"Subject\")", "time(\"Act_Time\")", "dose(A1<-\"Amount\")", "covr(Sex<-\"Gender\"(\"female\"=0, \"male\"=1))", "covr(Age<-\"Age\")", "covr(BW<-\"BodyWeight\")", "covr(group<-\"group\"(\"first_group\"=1, \"second_group\"=2, \"third_group\"=3))", "obs(CObs<-\"Conc\")", "table(file=\"posthoc.csv\", covr(Sex, Age, BW, group), V, Cl, V2, Cl2, mode=keep)", ""]
    }

