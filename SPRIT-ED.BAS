DECLARE SUB setsprite (col%)
DECLARE SUB setcolor (col%)
DECLARE SUB setspritepage (col%)
DECLARE SUB EmpanelSprite (x%(), el%)
DECLARE SUB SetSpriteEd (col%)
DECLARE SUB PALlETE ()
DECLARE SUB BMPFileLoad (BmpFiNo%)
DECLARE SUB LoadSprites (SpriteSet%)
DECLARE FUNCTION rgb% (x%)
TYPE regs
  a AS INTEGER
  B AS INTEGER
  c AS INTEGER
  d AS INTEGER
  x AS STRING * 12
END TYPE
DIM r AS regs

TYPE BMPHeaderType
        id AS STRING * 2      'Should be "BM"
        size AS LONG          'Size of the data
        rr1 AS INTEGER        '
        rr2 AS INTEGER        '
        offset AS LONG        'Position of start of pixel data
        horz AS LONG          '
        wid AS LONG           'Image width
        hei AS LONG           'Image height
        planes AS INTEGER     '
        bpp AS INTEGER        'Should read 8 for a 256 colour image
        pakbyte AS LONG       '
        imagebytes AS LONG    'Width*Height
        xres AS LONG          '
        yres AS LONG          '
        colch AS LONG         '
        ic AS LONG            '
        pal AS STRING * 64  'Stored as &amp;lt;Blue, Green, Red, 0&amp;gt;
END TYPE
DIM SHARED bmpheader AS BMPHeaderType


DIM SHARED SpriteFiles(2) AS STRING * 5
SpriteFiles(0) = "PAGA1"
SpriteFiles(1) = "PAGA2"
SpriteFiles(2) = "PAGA3"
DIM SHARED PgSet%
DIM SHARED pgsprites%(6899)
DIM PgVersion

SHELL "dir *.bmp/b>bmplist"
OPEN "bmplist" FOR INPUT AS #1
DIM SHARED bmpfiles(19) AS STRING * 8
DIM SHARED bmpfilen(19) AS INTEGER
DIM maxbmpfiles AS INTEGER


DO
        INPUT #1, a$
        bmpfilen(maxbmpfiles) = LEN(a$) - 4
        bmpfiles(maxbmpfiles) = LEFT$(a$, bmpfilen(maxbmpfiles))
        maxbmpfiles = maxbmpfiles + 1
LOOP UNTIL EOF(1) OR maxbmpfiles = 20


DIM SHARED rgbi%(15)
FOR t% = 0 TO 15
        rgbi%(t%) = rgb%(t%)
NEXT
SWAP rgbi%(7), rgbi%(8)
DIM SHARED BMPGrabArray%(69)

DIM SHARED SpriteEdX AS INTEGER
DIM SHARED SpriteEdY AS INTEGER

DIM SHARED SelectedSprite AS INTEGER
DIM SHARED SelectedCol AS INTEGER
DIM SHARED BmpFileNo%

SelectedSprite = 0
SCREEN 12
BMPFileLoad 0
LoadSprites 0
EmpanelSprite pgsprites%(), SelectedSprite
setsprite 15
PALlETE
setcolor 15

r.a = 1
CALL interruptx(&H33, r, r)
LOCATE 18, 71
PRINT "Sve Sprite";
LOCATE 20, 71
PRINT "*Save Set*";
LOCATE 22, 71
PRINT "*- Exit -*";
DO
r.a = 3
CALL interruptx(&H33, r, r)
x% = r.c
y% = r.d
IF r.B = 2 THEN
  r.a = 2
  CALL interruptx(&H33, r, r)
  IF y% > 420 THEN
    IF y% < 460 OR x% < 576 THEN
      Sprite% = 34 * INT((y% - 420) / 20) + INT(x% / 18)
      EmpanelSprite pgsprites%(), Sprite%
      setsprite 11
    END IF
  ELSE
      IF x% > 560 AND y% > 39 AND y% < 120 THEN
        setcolor 0
        SelectedCol = POINT(INT(x% / 5) * 5, INT(y% / 5) * 5)
        setcolor 15
      END IF
  END IF
  r.a = 1
  CALL interruptx(&H33, r, r)
END IF
IF r.B = 1 THEN
  r.a = 2
  CALL interruptx(&H33, r, r)
  IF y% < 420 THEN
    IF x% < 544 THEN
      GET (x%, y%)-STEP(15, 15), BMPGrabArray%
      EmpanelSprite BMPGrabArray%(), -1
      setsprite 11
    ELSEIF x% > 560 THEN
      IF y% < 16 THEN
        IF x% > 600 THEN
          BmpFileNo% = BmpFileNo% + 1
        ELSE
          BmpFileNo% = BmpFileNo% - 1
        END IF
        BmpFileNo% = (BmpFileNo% + maxbmpfiles) MOD maxbmpfiles
        BMPFileLoad BmpFileNo%
      ELSEIF y% < 32 THEN
        IF x% < 600 THEN
          PgSet% = PgSet% + 2
        ELSE
          PgSet% = PgSet% + 1
        END IF
        PgSet% = PgSet% MOD 3
        LoadSprites PgSet%
        setsprite 0
        SelectedSprite = 0
        setsprite 15
        EmpanelSprite pgsprites%(), SelectedSprite
      ELSEIF y% > 39 AND y% < 120 THEN
        setsprite 11
        celx% = INT((x% - 560) / 5)
        cely% = INT((y% - 40) / 5)
        LINE (560 + celx% * 5, 40 + cely% * 5)-STEP(3, 3), SelectedCol, BF
        PSET (560 + celx%, 240 + cely%), SelectedCol
      ELSEIF y% > 129 AND y% < 210 THEN
        setcolor 0
        SelectedCol = INT((x% - 560) / 20) + INT((y% - 130) / 20) * 4
        setcolor 15
      ELSEIF y% > 271 AND y% < 289 THEN
        GET (560, 240)-STEP(15, 15), pgsprites%(SelectedSprite * 69)
        PUT (1 + 18 * (SelectedSprite MOD 34), 421 + 20 * INT(SelectedSprite / 34)), pgsprites%(SelectedSprite * 69), PSET
        setsprite 15
        setspritepage 11
      ELSEIF y% > 303 AND y% < 321 THEN
        DEF SEG = VARSEG(pgsprites%(0))
        BSAVE SpriteFiles(SpriteSet%) + ".012", VARPTR(pgsprites%(0)), 13800
        setspritepage 15
      ELSEIF y% > 335 AND y% < 353 THEN
        END
      END IF

    END IF
    
  ELSE
    IF y% < 460 OR x% < 576 THEN
      setsprite 0
      SelectedSprite = 34 * INT((y% - 420) / 20) + INT(x% / 18)
      EmpanelSprite pgsprites%(), SelectedSprite
      setsprite 15
    END IF
  END IF
  r.a = 1
  CALL interruptx(&H33, r, r)
END IF
LOOP

SUB BMPFileLoad (BmpFiNo%)
        OPEN LEFT$(bmpfiles(BmpFiNo%), bmpfilen(BmpFiNo%)) + ".BMP" FOR BINARY AS #7
        GET #7, , bmpheader
        LOCATE 1, 71
        PRINT CHR$(27); bmpfiles(BmpFiNo%); CHR$(26)

iHeight% = bmpheader.hei - 1
iWidth% = INT((bmpheader.wid + 3) / 4) * 4 - 1
DIM Twopix AS STRING * 1
LINE (0, 0)-(559, 419), 0, BF

FOR y% = iHeight% TO 0 STEP -1
        FOR x% = 0 TO iWidth% STEP 2
                GET #7, , Twopix
                IF y% < 420 AND x% < 560 THEN
                        PSET (x% + 1, y%), rgbi%(ASC(Twopix) AND &HF)
                        PSET (x%, y%), rgbi%(INT(ASC(Twopix) / 16))
                END IF
        NEXT x%
NEXT y%
CLOSE #7

END SUB

SUB EmpanelSprite (x%(), el%)
IF el% > -1 THEN
ELSE
        el% = 0
END IF
PUT (560, 240), x%(el% * 69), PSET
FOR x% = 0 TO 15
FOR y% = 0 TO 15
        LINE (560 + x% * 5, 40 + y% * 5)-STEP(3, 3), POINT(560 + x%, 240 + y%), BF
NEXT
NEXT
END SUB

SUB LoadSprites (SpriteSet%)
DEF SEG = VARSEG(pgsprites%(0))
BLOAD SpriteFiles(SpriteSet%) + ".012", VARPTR(pgsprites%(0))
setspritepage 15
LINE (0, 420)-(639, 480), 0, BF
DIM Sprite%
FOR Sprite% = 0 TO 99
        PUT (1 + (Sprite% MOD 34) * 18, 421 + INT(Sprite% / 34) * 20), pgsprites%(Sprite% * 69)
NEXT
END SUB

SUB PALlETE
FOR col% = 0 TO 15
LINE (561 + (col% AND 3) * 20, 131 + 20 * INT(col% / 4))-STEP(18, 18), col%, BF
NEXT
END SUB

FUNCTION rgb% (x%)
        rgb = (x% AND 10) + (x% AND 4) / 4 + (x% AND 1) * 4

END FUNCTION

SUB setcolor (col%)
  LINE (560 + (SelectedCol AND 3) * 20, 131 + INT(SelectedCol / 4) * 20)-STEP(20, 20), col%, B


END SUB

SUB setsprite (col%)
LINE ((SelectedSprite MOD 34) * 18, 420 + INT(SelectedSprite / 34) * 20)-STEP(19, 19), col%, B
END SUB

SUB setspritepage (col%)
COLOR col%
LOCATE 2, 71
PRINT CHR$(27); SpriteFiles(PgSet%); "   "; CHR$(26)
END SUB

