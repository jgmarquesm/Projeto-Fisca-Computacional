MODULE leitura
!------------------------------------------------------------------------------------------------------------------
USE precisao
!------------------------------------------------------------------------------------------------------------------
IMPLICIT NONE
  REAL(ps),SAVE             :: t0,tf          ! Tempo incial e tempo final
  REAL(ps),SAVE             :: o10,o20        ! Ângulos inciais
  REAL(ps),SAVE             :: l10,l20        ! Posições inciais
  REAL(ps),SAVE             :: v10,v20        ! Velocidades inciais
  REAL(ps),SAVE             :: w10,w20        ! Velocidade angulares inciais
  REAL(ps),SAVE             :: l1,l2          ! Comprimento das hastes
  REAL(ps),SAVE             :: k1,k2          ! Constantes elásticas        
  REAL(ps),SAVE             :: m1,m2          ! Massa dos pêndulos 
  REAL(ps),SAVE             :: g              ! Aceleração da gravidade
  REAL(ps),SAVE             :: ut,um,uc
  INTEGER(ip3),SAVE         :: n              ! Número de iterações do método RK4
  INTEGER(ip2),SAVE         :: gl             ! Graus de liberdade do sistema (número de eq. dif. de 2º ordem)
  CHARACTER(LEN=20),SAVE    :: nome
!------------------------------------------------------------------------------------------------------------------
CONTAINS
!------------------------------------------------------------------------------------------------------------------
SUBROUTINE lercaso(caso,arq)
 IMPLICIT NONE
   INTEGER(ip1),INTENT(IN)       :: caso
   CHARACTER(LEN=20),INTENT(IN)  :: arq
OPEN(UNIT=20,FILE=arq,FORM='FORMATTED',STATUS='OLD')
    READ(20,*) t0
    READ(20,*) tf
    READ(20,*) o10
    READ(20,*) w10
    READ(20,*) o20
    READ(20,*) w20
    READ(20,*) l1
    READ(20,*) l2
    READ(20,*) m1
    READ(20,*) m2
    READ(20,*) g
    READ(20,*) n
    READ(20,*) gl
    READ(20,*) nome
    READ(20,*) ut
    READ(20,*) um
    READ(20,*) uc
! # Adimensionalização
t0  = t0/ut
tf  = tf/ut
w10 = w10*ut
w20 = w20*ut
l1  = l1/uc
l2  = l2/uc
m1  = m1/um
m2  = m2/um
g   = (g*ut*ut)/um
Select case(caso)
  case(1)
      Call ler1(arq)
  case(2)
      Call ler2(arq)
  case(3)
      Call ler3(arq)
  case(4)
      Call ler4(arq)
end select
!------------------------------------------------------------------------------------------------------------------   
END SUBROUTINE lercaso
!------------------------------------------------------------------------------------------------------------------
SUBROUTINE ler1(caso)
 IMPLICIT NONE
  CHARACTER (LEN=20),INTENT(IN) :: caso  
CLOSE(UNIT=20,STATUS='KEEP')
!------------------------------------------------------------------------------------------------------------------        
    PRINT*, ' '
    PRINT*, 'Leitura feita com sucesso. Os valores lidos são:'
    PRINT*, ' '
    PRINT*, 't0   =',t0,'s'
    PRINT*, 'tf   =',tf,'s'
    PRINT*, 'o10  =',o10,'rad'
    PRINT*, 'o20  =',o20,'rad'
    PRINT*, 'w10  =',w10,'rad/s'
    PRINT*, 'w20  =',w20,'rad/s'
    PRINT*, 'l1   =',l1,'m'
    PRINT*, 'l2   =',l2,'m'
    PRINT*, 'm1   =',m1,'Kg'
    PRINT*, 'm2   =',m2,'Kg'
    PRINT*, 'g    =',g,'m/s²'
    PRINT*, 'n    =',n
    PRINT*, 'gl   =',gl
    PRINT*, 'Nome =',nome
!------------------------------------------------------------------------------------------------------------------   
END SUBROUTINE ler1
!------------------------------------------------------------------------------------------------------------------
SUBROUTINE ler2(caso)
 IMPLICIT NONE
  CHARACTER (LEN=20),INTENT(IN)  :: caso
    READ(20,*) l10
    READ(20,*) v10
    READ(20,*) k1
! # Adimensionalização
l10  = l10/uc
v10  = (v10*ut)/uc
k1   = (k1*ut*ut)/um
CLOSE(UNIT=20,STATUS='KEEP')
!------------------------------------------------------------------------------------------------------------------        
    PRINT*, ' '
    PRINT*, 'Leitura feita com sucesso. Os valores lidos são:'
    PRINT*, ' '
    PRINT*, 't0   =',t0,'s'
    PRINT*, 'tf   =',tf,'s'
    PRINT*, 'o10  =',o10,'rad'
    PRINT*, 'o20  =',o20,'rad'
    PRINT*, 'w10  =',w10,'rad/s'
    PRINT*, 'w20  =',w20,'rad/s'
    PRINT*, 'l10  =',l10,'m'
    PRINT*, 'v10  =',v10,'m/s'
    PRINT*, 'k1   =',k1,'N/m'
    PRINT*, 'l1   =',l1,'m'
    PRINT*, 'l2   =',l2,'m'
    PRINT*, 'm1   =',m1,'Kg'
    PRINT*, 'm2   =',m2,'Kg'
    PRINT*, 'g    =',g,'m/s²'
    PRINT*, 'n    =',n
    PRINT*, 'gl   =',gl
    PRINT*, 'Nome =',nome
!------------------------------------------------------------------------------------------------------------------   
END SUBROUTINE ler2
!------------------------------------------------------------------------------------------------------------------
SUBROUTINE ler3(caso)
 IMPLICIT NONE
  CHARACTER (LEN=20),INTENT(IN) :: caso
    READ(20,*) l20
    READ(20,*) v20
    READ(20,*) k2
! # Adimensionalização
l20  = l20/uc
v20  = (v20*ut)/uc
k2   = (k2*ut*ut)/um
CLOSE(UNIT=20,STATUS='KEEP')
!------------------------------------------------------------------------------------------------------------------        
    PRINT*, ' '
    PRINT*, 'Leitura feita com sucesso. Os valores lidos são:'
    PRINT*, ' '
    PRINT*, 't0   =',t0,'s'
    PRINT*, 'tf   =',tf,'s'
    PRINT*, 'o10  =',o10,'rad'
    PRINT*, 'o20  =',o20,'rad'
    PRINT*, 'w10  =',w10,'rad/s'
    PRINT*, 'w20  =',w20,'rad/s'
    PRINT*, 'l1   =',l1,'m'
    PRINT*, 'l2   =',l2,'m'
    PRINT*, 'l20  =',l20,'m'
    PRINT*, 'v20  =',v20,'m/s'
    PRINT*, 'k2   =',k2,'N/m'
    PRINT*, 'm1   =',m1,'Kg'
    PRINT*, 'm2   =',m2,'Kg'
    PRINT*, 'g    =',g,'m/s²'
    PRINT*, 'n    =',n
    PRINT*, 'gl   =',gl
    PRINT*, 'Nome =',nome
!------------------------------------------------------------------------------------------------------------------   
END SUBROUTINE ler3
!------------------------------------------------------------------------------------------------------------------
SUBROUTINE ler4(caso)
 IMPLICIT NONE
  CHARACTER (LEN=20),INTENT(IN) :: caso    
    READ(20,*) l10
    READ(20,*) v10
    READ(20,*) k1
    READ(20,*) l20
    READ(20,*) v20
    READ(20,*) k2
! # Adimensionalização
l10  = l10/uc
v10  = (v10*ut)/uc
k1   = (k1*ut*ut)/um
l20  = l20/uc
v20  = (v20*ut)/uc
k2   = (k2*ut*ut)/um
CLOSE(UNIT=20,STATUS='KEEP')
!------------------------------------------------------------------------------------------------------------------        
    PRINT*, ' '
    PRINT*, 'Leitura feita com sucesso. Os valores lidos são:'
    PRINT*, ' '
    PRINT*, 't0   =',t0,'s'
    PRINT*, 'tf   =',tf,'s'
    PRINT*, 'o10  =',o10,'rad'
    PRINT*, 'o20  =',o20,'rad'
    PRINT*, 'w10  =',w10,'rad/s'
    PRINT*, 'w20  =',w20,'rad/s'
    PRINT*, 'l1  =',l1,'m'
    PRINT*, 'l10  =',l10,'m'
    PRINT*, 'v10  =',v10,'m/s'
    PRINT*, 'k1   =',k1,'N/m'
    PRINT*, 'l2   =',l2,'m'
    PRINT*, 'l20  =',l20,'m'
    PRINT*, 'v20  =',v20,'m/s'
    PRINT*, 'k2   =',k2,'N/m'
    PRINT*, 'm1   =',m1,'Kg'
    PRINT*, 'm2   =',m2,'Kg'
    PRINT*, 'g    =',g,'m/s²'
    PRINT*, 'n    =',n
    PRINT*, 'gl   =',gl
    PRINT*, 'Nome =',nome
!------------------------------------------------------------------------------------------------------------------   
END SUBROUTINE ler4
!------------------------------------------------------------------------------------------------------------------
END MODULE leitura
