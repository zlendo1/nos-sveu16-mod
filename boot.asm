  ORG 0
  LOD R2,R2,R15  	; komandni port
  WRD $FFFE
  SUB R0,R2,R2   	; Vrijednost 0
  STO R0,R0,R2   	; reset diska, kasnije R0 sadrži broj sektora
  EQU R1,R2,R2   	; 1 u R1
  LOD R4,R4,R15  	; Izbor sektora
  WRD $FFFD
  LOD R5,R5,R15  	; Prijenos podataka
  WRD $FFFC
  LOD R8,R8,R15  	; Početna adresa
  WRD $0100      	; Od lokacije 256
  LOD R3,R3,R15  	; Brojač sektora
  WRD 64         	; Mislim da je 64 sektora veličina forth.mem

  ORA R9,R15,R15 	; Zapamti programski brojač za vanjsku petlju
VANJSKA:
  STO R0,R0,R4   	; Selektuj sektor
  STO R1,R1,R2   	; Vrijednost 1 je komanda Read
  LOD R11,R11,R15 ; Brojač 16 bitnih riječi u sektoru
  WRD 128         ; 128
  ORA R10,R15,R15 ; Zapamti programski brojač za  unutrašnju petlju
UNUTRASNJA:
  LOD R6,R6,R5    ; Sljedećih 16 bita sa diska
  STO R6,R6,R8    ; Upiši ih u memoriju
  ADD R8,R8,R1    ; Uvećaj pokazivač memorije
  SUB R11,R11,R1  ; Umanji brojač u sektoru
  LTU R7,R11,R1   ; Kada on postane 0 R7=1
  MIF R15,R7,R10  ; Uslovni skok na unutrašnju petlju
  ADD R0,R0,R1    ; Sljedeći sektor
  SUB R3,R3,R1    ; Umanji brojač sektora
  LTU R7,R3,R1    ; Kada on postane 0 R7=1
  MIF R15,R7,R9   ; Uslovni skok na vanjsku petlju
  LOD R15,R15,R15 ; Skok u operativni sistem
  WRD $100 
