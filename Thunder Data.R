y=as.numeric(shots_data$y)
x=as.numeric(shots_data$x)
fgmade=as.numeric(shots_data$fgmade)

NC3.A=numeric(nrow(shots_data))
C3.A=numeric(nrow(shots_data))
TwoPT.A=numeric(nrow(shots_data))
NC3.B=numeric(nrow(shots_data))
C3.B=numeric(nrow(shots_data))
TwoPT.B=numeric(nrow(shots_data))

NC3.A.Make=numeric(nrow(shots_data))
C3.A.Make=numeric(nrow(shots_data))
TwoPT.A.Make=numeric(nrow(shots_data))
NC3.B.Make=numeric(nrow(shots_data))
C3.B.Make=numeric(nrow(shots_data))
TwoPT.B.Make=numeric(nrow(shots_data))


for(r in 1:nrow(shots_data))
{
  if(shots_data$team[r]=="Team A")
  {
    C3.A[r]=1*((x[r]>22 || x[r]<(-22)) && y[r]<=7.8)
    TwoPT.A[r]=1*((x[r]<=22 && x[r]>=(-22) && y[r]<=7.8) || 
                  (x[r]^2+y[r]^2<=23.75^2 && y[r]>7.8))
    NC3.A[r]=1*(x[r]^2+y[r]^2>23.75^2 && y[r]>7.8)
  }
  
  if(shots_data$team[r]=="Team B")
  {
    C3.B[r]=1*((x[r]>22 || x[r]<(-22)) && y[r]<=7.8)
    TwoPT.B[r]=1*((x[r]<=22 && x[r]>=(-22) && y[r]<=7.8) || 
                    (x[r]^2+y[r]^2<=23.75^2 && y[r]>7.8))
    NC3.B[r]=1*(x[r]^2+y[r]^2>23.75^2 && y[r]>7.8)
  }

  if(shots_data$team[r]=="Team A")
  {
    C3.A.Make[r]=1*((x[r]>22 || x[r]<(-22)) && y[r]<=7.8 && fgmade[r]==1)
    TwoPT.A.Make[r]=1*((x[r]<=22 && x[r]>=(-22) && y[r]<=7.8 && fgmade[r]==1) || 
                  (x[r]^2+y[r]^2<=23.75^2 && y[r]>7.8 && fgmade[r]==1))
    NC3.A.Make[r]=1*(x[r]^2+y[r]^2>23.75^2 && y[r]>7.8 && fgmade[r]==1)
  }
  
  if(shots_data$team[r]=="Team B")
  {
    C3.B.Make[r]=1*((x[r]>22 || x[r]<(-22)) && y[r]<=7.8 && fgmade[r]==1)
    TwoPT.B.Make[r]=1*((x[r]<=22 && x[r]>=(-22) && y[r]<=7.8 && fgmade[r]==1) || 
                       (x[r]^2+y[r]^2<=23.75^2 && y[r]>7.8 && fgmade[r]==1))
    NC3.B.Make[r]=1*(x[r]^2+y[r]^2>23.75^2 && y[r]>7.8 && fgmade[r]==1)
  }
}

C3.A.Att=sum(C3.A)
TwoPT.A.Att=sum(TwoPT.A)
NC3.A.Att=sum(NC3.A)
C3.B.Att=sum(C3.B)
TwoPT.B.Att=sum(TwoPT.B)
NC3.B.Att=sum(NC3.B)

C3.A.Made=sum(C3.A.Make)
TwoPT.A.Made=sum(TwoPT.A.Make)
NC3.A.Made=sum(NC3.A.Make)
C3.B.Made=sum(C3.B.Make)
TwoPT.B.Made=sum(TwoPT.B.Make)
NC3.B.Made=sum(NC3.B.Make)

FGA.A=C3.A.Att+TwoPT.A.Att+NC3.A.Att
FGA.B=C3.B.Att+TwoPT.B.Att+NC3.B.Att

##FGM.A=C3.A.Made+TwoPT.A.Made+NC3.A.Made
##FGM.B=C3.B.Made+TwoPT.B.Made+NC3.B.Made

##ThreePM.A=C3.A.Made+NC3.A.Made
##ThreePM.B=C3.B.Made+NC3.B.Made

eFG.C3.A=((C3.A.Made+(0.5*C3.A.Made))/C3.A.Att)
eFG.TwoPT.A=((TwoPT.A.Made+(0.5*0))/TwoPT.A.Att)
eFG.NC3.A=((NC3.A.Made+(0.5*NC3.A.Made))/NC3.A.Att)
eFG.C3.B=((C3.B.Made+(0.5*C3.B.Made))/C3.B.Att)
eFG.TwoPT.B=((TwoPT.B.Made+(0.5*0))/TwoPT.B.Att)
eFG.NC3.B=((NC3.B.Made+(0.5*NC3.B.Made))/NC3.B.Att)

eFG.C3.A
eFG.TwoPT.A
eFG.NC3.A
eFG.C3.B
eFG.TwoPT.B
eFG.NC3.B

ShotDist.C3.A=C3.A.Att/(C3.A.Att+TwoPT.A.Att+NC3.A.Att)
ShotDist.TwoPT.A=TwoPT.A.Att/(C3.A.Att+TwoPT.A.Att+NC3.A.Att)
ShotDist.NC3.A=NC3.A.Att/(C3.A.Att+TwoPT.A.Att+NC3.A.Att)
ShotDist.C3.B=C3.B.Att/(C3.B.Att+TwoPT.B.Att+NC3.B.Att)
ShotDist.TwoPT.B=TwoPT.B.Att/(C3.B.Att+TwoPT.B.Att+NC3.B.Att)
ShotDist.NC3.B=NC3.B.Att/(C3.B.Att+TwoPT.B.Att+NC3.B.Att)

ShotDist.C3.A
ShotDist.TwoPT.A
ShotDist.NC3.A
ShotDist.C3.B
ShotDist.TwoPT.B
ShotDist.NC3.B

