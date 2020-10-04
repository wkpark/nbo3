C******************************************************************************
      PROGRAM ENABLE
C******************************************************************************
      IMPLICIT REAL (A-H,O-Z)
C
      PARAMETER(NID = 8)
C
      CHARACTER*80 STRING
      CHARACTER*10 NAME
      CHARACTER*3  ID,LID, IDENT(NID)
      CHARACTER*1  STAR,BLANK
      character*5 fmt
C
      DATA IDENT      /'GEN','AMP','GMS','HND','G82','G86','G88','G90'/
      DATA NAME       /'XXXnbo.f'/
      DATA STAR,BLANK /'*',' '/
C
      DATA LFNIN,LFNOUT,LFNSRC,LFNFOR/5,6,7,8/
C
C  WHICH VERSION OF THE NBO PROGRAM SHOULD BE ENABLE?
C
   10 WRITE(LFNOUT,900)
      READ(LFNIN,1000) ID

      ! capitalize ID, and get lowercase ID as LID
      loa = ichar('a')
      loz = ichar('z')
      hia = ichar('A')
      hiz = ichar('Z')
      do i = 1, 3
        j = ichar(id(i:i))
        if (j.ge.loa.and.j.le.loz) then
          lid(i:i) = id(i:i)
          j = j - loa + hia
          id(i:i) = char(j)
        else if (j.ge.hia.and.j.le.hiz) then
          j = j - hia + loa
          lid(i:i) = char(j)
        end if
      end do
C
C  MAKE SURE THIS IDENTIFIER IS RECOGNIZED:
C
      IFLG = 0
      DO 20 I = 1,NID
        IF(IDENT(I).EQ.ID) IFLG = I
   20 CONTINUE
      IF(IFLG.EQ.0) GOTO 10
C
C  OPEN THE INPUT NBO SOURCE FILE AND THE OUTPUT FORTRAN FILE:
C
      OPEN(UNIT=LFNSRC, FILE='nbo.src.f', STATUS='OLD', ERR=800)
C
      NAME(1:3) = lid
      OPEN(UNIT=LFNFOR, FILE=NAME, STATUS='UNKNOWN')
C
C  READ SOURCE CODE, WRITING OUT LINES LABELLED WITH THE APPROPRIATE
C  IDENTIFIER:
C
      ICNT = 0
   30 ICNT = ICNT + 1
      READ(LFNSRC,1010,ERR=810,END=50) STRING
C
C  IF THE FIRST CHARACTER OF A LINE IS A '*' AND THE LINE IS LABELLED
C  BY 'ID', REMOVE THE '*' (COMMENT):
C
      IF(STRING(1:1).EQ.STAR) THEN
        IF(STRING(73:75).EQ.ID) THEN
          STRING(1:1) = BLANK
C
C  IF THE FIRST CHARACTER IS A '*' AND THE LABELLED IS UNRECOGNIZED,
C  HALT PROGRAM EXECUTION:
C
        ELSE
          JFLG = 0
          DO 40 I = 1,NID
            IF(STRING(73:75).EQ.IDENT(I)) JFLG = I
   40     CONTINUE
          IF(JFLG.EQ.0) GOTO 820
        END IF
      END IF
C
C  WRITE THIS LINE TO THE FORTRAN FILE:
C
      L = 80
      do J = 80, 1, -1
        if (string(j:j).eq.' ') then
          string(j:j)=char(0)
          L = L - 1
        else
          exit
        endif
      end do
      FMT(1:2) = '(A'
      if (L.GE.10) then
        FMT(3:3) = char((L / 10) + ichar('0'))
        FMT(4:4) = char((mod(L,10)) + ichar('0'))
        FMT(5:5) = ')'
      else
        FMT(3:3) = char(L + ichar('0'))
        FMT(4:4) = ')'
      end if
      WRITE(LFNFOR,fmt) STRING
      GOTO 30
C
C  FINISH UP:
C
   50 ICNT = ICNT - 1
      WRITE(LFNOUT,950) ICNT,NAME
      CLOSE(LFNSRC)
      CLOSE(LFNFOR)
      CALL EXIT
C
  800 WRITE(LFNOUT,910)
      STOP
C
  810 WRITE(LFNOUT,920) ICNT
      STOP
C
  820 WRITE(LFNOUT,930) STRING(73:75),ICNT
      STOP
C
  900 FORMAT(1X,'NBO Program version to enable?  ')
  910 FORMAT(1X,'NBO source code (NBO.SRC) is not found.')
  920 FORMAT(1X,'Error reading from NBO.SRC as line ',I5,'.')
  930 FORMAT(1X,'Unknown version label (',A3,') at line ',I5,
     + ' for nbo.src.')
  940 FORMAT(A80)
  950 FORMAT(1X,I5,' lines written to ',A10,'.')
 1000 FORMAT(A3)
 1010 FORMAT(A80)
      END
