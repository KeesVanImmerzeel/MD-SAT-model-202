library dsmodel202;
  {-Afvoermodel Kraijenhoff van de Leur: berekening van de specifieke afvoer.
    Ref.: A study of non-steady grondwater flow with special reference to a
    reservoir coefficient. De Ingenieur 70:: B87-B94.Kraijenhoff van de Leur,
    D.A. (1958) }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$Define Debug}

uses
  ShareMem,
  windows, SysUtils, Classes, LargeArrays, ExtParU, USpeedProc, uDCfunc,
  UStepRout, UdsModel, UdsModelS, xyTable, DUtils, uError;

Const
  cModelID      = 202;  {-Uniek modelnummer}

  {-Beschrijving van de array met afhankelijke variabelen}
  cNrOfDepVar   = 2;    {-Lengte van de array met afhankelijke variabelen}
  cQcum         = 1;    {-Cumulatieve specifieke afvoer (m/d); t.b.v. berekening
                          van GEMIDDELDE specifieke afvoer}
  cGWScum       = 2;    {-Cumulatieve grondwaterstand (m-mv) midden tussen de
                          sloten; t.b.v. berekening van GEMIDDELDE grondwaterstand
                          (m-mv). }

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {-Variabelen die samenhangen met het aanroepen van het model vanuit de Shell}
  cnRP    = 7;   {-Aantal RP-tijdreeksen die door de Shell moeten worden aangeleverd (in
                   de externe parameter Array EP (element EP[ indx-1 ]))}
  cnSQ    = 0;   {-Idem punt-tijdreeksen}
  cnRQ    = 0;   {-Idem lijn-tijdreeksen}

  {-Beschrijving van het eerste element van de externe parameter-array (EP[cEP0])}
  cNrXIndepTblsInEP0 = 3;    {-Aantal XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;    {-Aantal Xdep-tables   in EP[cEP0]}
  {-Nummering van de xIndep-tabellen in EP[cEP0]. De nummers 0&1 zijn gereserveerd}
  cTb_MinMaxValKeys   = 2;

  {-Beschrijving van het tweede element van de externe parameter-array (EP[cEP1])}
  {-Opmerking: table 0 van de xIndep-tabellen is gereserveerd}
  {-Nummering van de xdep-tabellen in EP[cEP1]}
  cTb_NN             = 0;
  cTb_ReservoirCoeff = 1; 
  cTb_mu             = 2;
  cTb_SltAfst        = 3;
  cTb_SltPeil        = 4;
  cTb_Mv             = 5;
  cTb_Q_Init         = 6;

  {-Model specifieke fout-codes: -9951..-9999}
  cInvld_NN             = -9951;
  cInvld_ReservoirCoeff = -9952;
  cInvld_mu             = -9953;
  cInvld_SltAfst        = -9954;
  cInvld_SltPeil        = -9955;
  cInvld_Mv             = -9956;
  cInvld_Q_Init         = -9957;

  {-Overige constanten}
  cArrayLen = 15; {-Aantal 'vaten' waaruit het Kraijenhoff van de Leur model
                    bestaat}

Type
  T_KdL_Array = Array[1..cArrayLen] of Double; {-Array waarvan de lengte gelijk is a.h.
                                         aantal vaten in Kraijenhoff van de Leur model}

var
  Indx: Integer; {-Door de Boot-procedure moet de waarde van deze index worden ingevuld,
                   zodat de snelheidsprocedure 'weet' waar (op de externe parameter-array)
                   hij zijn gegevens moet zoeken}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
                 (zie nDC) }

  {-Globally defined parameters}
  {-Parameters from shell}

  An, Bn, Jn_ruw,   {-Coefficienten behorende bij elk vat}
  Q_InitArr,        {-Specifieke afvoer van elk vat a.h. begin v.h. tijdsinterval (t=x)}
  m0_InitArr        {-Bijdrage aan opbolling m0 van elk vat (m)}
  : T_KdL_Array;
  SumAn, SumBn, hlp: Double;
  i, Teken: Integer;
  {$ifdef Debug}
  lf: TextFile;
  {$endif}


   {-Geldige range van key-/parameter/initiele waarden. De waarden van deze  variabelen moeten
    worden ingevuld door de Boot-procedure}
  cMin_NN,             cMax_NN,
  cMin_ReservoirCoeff, cMax_ReservoirCoeff,
  cMin_mu,             cMax_mu,
  cMin_SltAfst,        cMax_SltAfst,
  cMin_SltPeil,        cMax_SltPeil,
  cMin_Mv,             cMax_Mv,
  cMin_Q_Init,         cMax_Q_Init: Double;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
    if ( nDC > 0 ) then
      ModelProfile.Free;
    {$ifdef Debug}
    {$I-} CloseFile( lf ); {$I+}
    {$endif}
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Deze procedure verschaft de array met afgeleiden 'dydx', gegeven het tijdstip 'x' en
  de toestand die beschreven wordt door de array 'y' en de externe condities die beschreven
  worden door de 'external parameter-array EP'. Als er geen fout op is getreden bij de
  berekening van 'dydx' dan wordt in deze procedure de variabele 'IErr' gelijk gemaakt aan de
  constante 'cNoError'. Opmerking: in de array 'y' staan dus de afhankelijke variabelen,
  terwijl 'x' de onafhankelijke variabele is (meestal de tijd)}
var
  {-Shell parameters}
  NN,                      {-Neerslagoverschot (m/d)}
  ReservoirCoeff,          {-Reservoir-coefficient (d)}
  mu,                      {-Freatische bergingscoefficient (-)}
  SltAfst,                 {-Slootafstand (m)}
  SltPeil,                 {-Slootpeil (m+NAP)}
  Mv,                      {-Maaiveldsniveau (m+NAP)}

  {-Calculated parameters}
  Currnt_xs,               {-Tijdstap-grootte (d)}
  Ckl,                     {-Bergingscapaciteit (m)}
  Wkl,                     {-Horizontale weerstand v.d. neerslagstroming nr. de sloot (d/m)}
  AvQ,                     {-Gem. toestroming in tijdsinterval x..x+Current_xs (m/d)}
  Avm0: Double;            {-Gem. opbolling in tijdsinterval x..x+Current_xs (m)}
  Jn: T_KdL_Array;         {-Tijdscoefficient behorende bij ieder vat (d)}

Function SetKeyAndParValues( var IErr: Integer ): Boolean;
  Function GetNN( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_NN ].EstimateY( x, Direction );
  end;
  Function GetReservoirCoeff( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_ReservoirCoeff ].EstimateY( x, Direction );
  end;
  Function Getmu( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_mu ].EstimateY( x, Direction );
  end;
  Function GetSltAfst( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_SltAfst ].EstimateY( x, Direction );
  end;
  Function GetSltPeil( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_SltPeil ].EstimateY( x, Direction );
  end;
  Function GetMv( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Mv ].EstimateY( x, Direction );
  end;
begin {-Function SetKeyAndParValues}
  Result := False;

  NN := GetNN( x );
  if ( NN < cMin_NN ) or ( NN > cMax_NN ) then begin
    IErr := cInvld_NN; Exit;
  end;
  ReservoirCoeff := GetReservoirCoeff( x );
  if ( ReservoirCoeff < cMin_ReservoirCoeff ) or ( ReservoirCoeff > cMax_ReservoirCoeff ) then begin
    IErr := cInvld_ReservoirCoeff; Exit;
  end;
  mu := Getmu( x );
  if ( mu < cMin_mu ) or ( mu > cMax_mu ) then begin
    IErr := cInvld_mu; Exit;
  end;
  SltAfst := GetSltAfst( x );
  if ( SltAfst < cMin_SltAfst ) or ( SltAfst > cMax_SltAfst ) then begin
    IErr := cInvld_SltAfst; Exit;
  end;
  SltPeil := GetSltPeil( x );
  if ( SltPeil < cMin_SltPeil ) or ( SltPeil > cMax_SltPeil ) then begin
    IErr := cInvld_SltPeil; Exit;
  end;
  Mv := GetMv( x );
  if ( Mv < cMin_Mv ) or ( Mv > cMax_Mv ) then begin
    IErr := cInvld_Mv; Exit;
  end;

  {-Opmerking: Q_Init wordt alleen overgenomen van de Shell bij de start van
    de simulatie (zie: 'Replace_InitialValues_With_ShellValues')}

  Ckl := ( 8 / Sqr( pi ) ) * mu * SltAfst;
  Wkl := ReservoirCoeff / Ckl;

  {$ifdef Debug}
  Writeln( lf, 'Values in "SetKeyAndParValues" (x=', x:6:2, '):' );
  Writeln( lf, 'NN            =', NN:8:4 );
  Writeln( lf, 'ReservoirCoeff=', ReservoirCoeff:8:4 );
  Writeln( lf, 'mu            =', mu:8:4 );
  Writeln( lf, 'SltAfst       =', SltAfst:8:4 );
  Writeln( lf, 'SltPeil       =', SltPeil:8:4 );
  Writeln( lf, 'Mv            =', mv:8:4 );
  Writeln( lf, 'Ckl           =', Ckl:8:4 );
  Writeln( lf, 'Wkl           =', Wkl:8:4 );
  Writeln( lf );
  {$endif}
  Result := True; IErr := cNoError;
end; {-Function SetKeyAndParValues}

Function Replace_InitialValues_With_ShellValues( var IErr: Integer): Boolean;
  {-Als de Shell 1-of meer initiele waarden aanlevert voor de array met afhankelijke
    variabelen ('y'), dan kunnen deze waarden hier op deze array worden geplaatst en
    gecontroleerd}
var
  Q_Init,           {-Specifieke afvoer a.h. begin v.h. tijdsinterval (t=x)}
  m0_Init: Double;  {-Initiele opbolling tussen de sloten (m)}
  i: Integer;
begin
  IErr := cNoError; Result := True;
  with EP[ indx-1 ].xDep do begin
    Q_Init     := Items[ cTb_Q_Init ].EstimateY( 0, Direction ); {Opm.: x=0}
    y[ cQcum ] := Q_Init;
  end;
  if ( y[ cQcum ] < cMin_Q_Init ) or
     ( y[ cQcum ] > cMax_Q_Init ) then begin
    IErr := cInvld_Q_Init; Result := False; Exit;
  end;
  m0_Init      := Q_Init * SltAfst * Wkl; {-Opbolling bij evenwicht NN en Q (t=oneindig)}
  y[ cGWScum ] := mv - (SltPeil + m0_Init);
  for i:=1 to cArrayLen do begin
    Q_InitArr[ i ]  := Q_Init  * An[ i ];
    m0_InitArr[ i ] := m0_Init * Bn[ i ];
  end;
  {$ifdef Debug}
  Writeln( lf, 'Values in "Replace_InitialValues_With_ShellValues" (x=', x:6:2, '):' );
  Writeln( lf, 'Q_Init         =', Q_Init:8:4 );
  Writeln( lf, 'm0_Init        =', m0_Init:8:4 );
  Writeln( lf, 'Q_InitArr, m0_InitArr:' );
  for i:=1 to cArrayLen do
    Writeln(lf, i:2, ' ', Q_InitArr[ i ]:10, ' ', m0_InitArr[ i ]:10 );
  Writeln( lf );
  {$endif}
end;

Function Current_Q: Double;
var i: Integer;
begin
  Result := 0;
  for i:=1 to cArrayLen do
    Result := Result + Q_InitArr[ i ];
end;

Function Current_m0: Double;
var i: Integer;
begin
  Result := 0;
  for i:=1 to cArrayLen do
    Result := Result + m0_InitArr[ i ];
end;

Procedure Finish_Jn_Array;
var
  i: Integer;
begin
  for i:=1 to cArrayLen do
    Jn[ i ] := Jn_ruw[ i ] * ReservoirCoeff;
  {$ifdef Debug}
  Writeln( lf, 'Values in "Finish_Jn_Array" (x=', x:6:2, '):' );
  for i:=1 to cArrayLen do
    Writeln( lf, i:2, ' ', Jn[ i ]:10:4 );
  Writeln( lf );
  {$endif}
end;

Function SetAvQ( const Q_InitArr: T_KdL_Array; const NN, hh: Double ): Boolean;
var
  i: Integer;
  NextQ: Double;
begin
  Result := True;
  AvQ := 0;
  {$ifdef Debug}
  Writeln( lf, 'Values "NextQ" in "SetAvQ" (x=', x:6:2, '):' );
  {$endif}
  for i:=1 to cArrayLen do begin
    NextQ := Q_InitArr[ i ] * Jn[ i ] *  ( 1 -  Exp( -hh / Jn[ i ] ) ) +
    NN * an[ i ] * ( hh + Jn[ i ] * ( (Exp( -hh / Jn[ i ] )) - 1 ) );
    {$ifdef Debug}
    Writeln( lf, i:2, ' ', NextQ:10:6 );
    {$endif}
    AvQ := AvQ + NextQ;
  end;
  AvQ := AvQ / hh;
  {$ifdef Debug}
  Writeln( lf, 'AvQ: ', AvQ:10:6 );
  {$endif}
end;

Function SetAvm0( const m0_InitArr: T_KdL_Array; const NN, hh: Double ): Boolean;
var
  i: Integer;
  Nextm0: Double;
begin
  Result := True;
  Avm0 := 0;
  {$ifdef Debug}
  Writeln( lf, 'Values "Nextm0" in "SetAvm0" (x=', x:6:2, '):' );
  {$endif}
  for i:=1 to cArrayLen do begin
    Nextm0 := m0_InitArr[ i ] * Jn[ i ] * ( 1 - Exp( -hh / Jn[ i ] ) ) +
    NN * SltAfst * Wkl * bn[ i ] * ( hh + Jn[ i ] * ( (Exp( -hh / Jn[ i ] )) - 1 ) );
    {$ifdef Debug}
    Writeln( lf, i:2, ' ', Nextm0:10:6 );
    {$endif}
    Avm0 := Avm0 + Nextm0;
  end;
  Avm0 := Avm0 / hh;
  {$ifdef Debug}
  Writeln( lf, 'Avm0: ', Avm0:10:6 );
  {$endif}
end;

Function SetNext_Q_InitArr( var Q_InitArr: T_KdL_Array; NN, hh: Double ): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i:=1 to cArrayLen do begin
    Q_InitArr[ i ] := Q_InitArr[ i ] *       exp( -hh / jn[ i ] ) +
                      NN * An[ i ]   * ( 1 - exp( -hh / jn[ i ] ) );
  end;
end;

Function SetNext_m0_InitArr( var m0_InitArr: T_KdL_Array; NN, hh: Double ): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i:=1 to cArrayLen do begin
    m0_InitArr[ i ] := m0_InitArr[ i ] *                     exp( -hh / jn[ i ] ) +
                       NN * SltAfst * Wkl * Bn[ i ] *  ( 1 - exp( -hh / jn[ i ] ) );
  end;
end;

begin
  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;
	
  {$ifdef Debug}
  if ( aModelProfile = NIL ) then
    Writeln( lf, 'No discontinuities' )
  else 
    Writeln( lf, 'Discontinuities present' );
  {$endif}

  if not SetKeyAndParValues( IErr ) then
    Exit;

  if ( Context = UpdateYstart ) then begin {-Run fase 1}

    {-Optioneel: initiele waarden vervangen door Shell-waarden}
    if not Replace_InitialValues_With_ShellValues( IErr ) then
      Exit;

    IErr := cNoError;
  end else begin {-Run fase 2}

    Currnt_xs := Current_xs( EP );
    {$ifdef Debug}
    Writeln( lf, 'x, Current_xs= ', x:8:2, ' ', Currnt_xs:8:2 );
    {$endif}

    if ( Currnt_xs > 0 ) then begin
      Finish_Jn_Array; {-Jn_Array bevat van ieder vat de tijdscoefficient}
      if not SetAvQ( Q_InitArr, NN, Currnt_xs ) then;
      if not SetAvm0( m0_InitArr, NN, Currnt_xs ) then;
      dydx[ cQcum ]   := AvQ;
      dydx[ cGWScum ] := mv - (SltPeil + Avm0);
      if not SetNext_Q_InitArr(   Q_InitArr, NN, Currnt_xs ) then;
      if not SetNext_m0_InitArr( m0_InitArr, NN, Currnt_xs ) then;
    end else begin {-Geef momentane snelheid}
      dydx[ cQcum ]   := Current_Q; {m/d}
      dydx[ cGWScum ] := mv - (SltPeil + Current_m0); {m}
      {$ifdef Debug}
      Writeln( lf, 'Current_Q, Current_m0: ', Current_Q:10:4, Current_m0:10:4 );
      {$endif}
    end;

  end; {-Run fase 2}
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Initialiseer de meest elementaire gegevens van het model. Shell-gegevens worden door deze
    procedure NIET verwerkt}
Procedure SetMinMaxKeyAndParValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
  cMin_NN              := GetValue( 1, 1 );
  cMax_NN              := GetValue( 1, 2 );
  cMin_ReservoirCoeff  := GetValue( 1, 3 );
  cMax_ReservoirCoeff  := GetValue( 1, 4 );
  cMin_mu              := GetValue( 1, 5 );
  cMax_mu              := GetValue( 1, 6 );
  cMin_SltAfst         := GetValue( 1, 7 );
  cMax_SltAfst         := GetValue( 1, 8 );
  cMin_SltPeil         := GetValue( 2, 1 );
  cMax_SltPeil         := GetValue( 2, 2 );
  cMin_Mv              := GetValue( 2, 3 );
  cMax_Mv              := GetValue( 2, 4 );
  cMin_Q_Init          := GetValue( 2, 5 );
  cMax_Q_Init          := GetValue( 2, 6 );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0,
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then begin
    SetMinMaxKeyAndParValues;
    SetAnalytic_DerivsProc( True, EP ); {-Ref. 'USpeedProc.pas'}
  end;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze boot-procedure verwerkt alle basisgegevens van het model en leest de Shell-gegevens
    uit een bestand. Na initialisatie met deze boot-procedure is het model dus gereed om
	'te draaien'. Deze procedure kan dus worden gebruikt om het model 'los' van de Shell te
	testen}
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then
    exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx, EP );
  if ( Result <> cNoError ) then
    exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze procedure maakt het model gereed voor Shell-gebruik.
    De xDep-tables in EP[ indx-1 ] worden door deze procedure NIET geinitialiseerd omdat deze
	gegevens door de Shell worden verschaft }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}

begin
  {-Dit zgn. 'DLL-Main-block' wordt uitgevoerd als de DLL voor het eerst in het geheugen wordt
    gezet (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );

  {$ifdef Debug}
  {$I-} AssignFile( lf, 'DebugDsModel202.log' ); Rewrite( lf );{$I+}
  {$endif}

   {-Fill Array's}
  SumAn := 0;
  SumBn := 0;
  Teken := 1;
  for i:=1 to cArrayLen do begin
    An[ i ] := 8 / ( Sqr(pi * ( 2 * i - 1 )) );
    SumAn := SumAn + An[ i ];
    hlp := ( pi * ( 2 * i - 1 ) );
    Bn[ i ] := ( 32 * Teken ) / ( hlp * hlp * hlp);
    Teken := - Teken;
    SumBn := SumBn + Bn[ i ];
    Jn_ruw[ i ] := 1 / ( Sqr(2 * i - 1) );
  end;
  An[ 1 ] := An[ 1 ] + ( 1 - SumAn );
  Bn[ 1 ] := Bn[ 1 ] + ( 1 - SumBn );

  {$ifdef Debug}
  Writeln( lf, 'An, Bn arrays:' );
  for i:=1 to cArrayLen do
    Writeln( lf, i:2, ' ', An[ i ]:8:4, ' ', Bn[ i ]:8:4 );
  Writeln( lf );
  {$endif}
end.
