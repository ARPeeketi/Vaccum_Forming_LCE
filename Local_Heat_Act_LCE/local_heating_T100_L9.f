      SUBROUTINE UTEMP(TEMP,NSECPT,KSTEP,KINC,TIME,NODE,COORDS)
C
      INCLUDE 'ABA_PARAM.INC'
	  

C
        
      DIMENSION TEMP(NSECPT), TIME(2), COORDS(3)
      REAL :: TMAX = 100.0, LENGTH = 9.0
C
      IF (COORDS(1).GT.(LENGTH))  THEN
          DO i=1,NSECPT,1
              TEMP(i)= (25.0 + (TMAX-25.0)*TIME(1)/10.0)
          END DO
	  ELSE IF (COORDS(1).GT.(0.0))  THEN
          DO i=1,NSECPT,1
              TEMP(i)= 25.0 + (TMAX-25.0)*(TIME(1)/10.0)*(COORDS(1)/LENGTH)
          END DO      
      ELSE
          DO i=1,NSECPT,1
              TEMP(i)= 25.0
          END DO        
      END IF
	  
      RETURN
      END
	  
