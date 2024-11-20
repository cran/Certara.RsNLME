# covariate effect is removed from model

    "test(){\n    cfMicro(A1,Cl/V, Cl2/V, Cl2/V2)\n    dosepoint(A1)\n    C = A1 / V\n    error(CEps=0.1)\n    observe(CObs=C * ( 1 + CEps))\n    stparm(V = tvV * (Age^dVdAge)   * exp(nV))\n    stparm(Cl = tvCl * ((BW/70)^dCldBW)   * exp(nCl + nClx1*(Sex==1) + nClx2*(Sex==2) ))\n    stparm(V2 = tvV2 * exp(nV2))\n    stparm(Cl2 = tvCl2 * exp(nCl2))\n    fcovariate(BW)\n    fcovariate(Age)\n    covariate(Sex)\n    fixef( tvV = c(,1,))\n    fixef( tvCl = c(,1,))\n    fixef( tvV2 = c(,1,))\n    fixef( tvCl2 = c(,1,))\n    fixef( dVdAge(enable=c(0)) = c(,0,))\n    fixef( dCldBW(enable=c(1)) = c(,0,))\n    ranef(diag(nV,nCl,nV2,nCl2) =  c(1,1,1,1))\n    ranef(diag(nClx1) = c(1), same(nClx2))\n}"

