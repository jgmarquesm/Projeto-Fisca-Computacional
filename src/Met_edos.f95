MODULE Met_edos
!------------------------------------------------------------------------------------------------------------------
  USE precisao
!------------------------------------------------------------------------------------------------------------------
  REAL(pr), DIMENSION(:,:), ALLOCATABLE, private :: k                ! Coeficientes usados no método RK4
CONTAINS
!---------------------------------------------------------------------------------------------  
  SUBROUTINE alocar_k(ngl) 
    USE precisao
    implicit none
    INTEGER(ip2),INTENT(IN) :: ngl
    INTEGER(ip2)            :: AllocateStatus !verificação de êxito na alocalção
! # Alocando k 
    ALLOCATE(k(1:4,1:2*ngl), Stat=Allocatestatus)
    IF(Allocatestatus /= 0) PRINT*, '===> Erro na alocação dos coeficientes de RK4 <===' 
  END SUBROUTINE alocar_k
!---------------------------------------------------------------------------------------------
  SUBROUTINE desaloca_k(AllocateStatus) 
    USE precisao
    implicit none
    INTEGER(ip3), intent(out) :: AllocateStatus !verificação de êxito na alocalção
! # Desalocando k 
    DEALLOCATE(k, Stat=Allocatestatus)
  END SUBROUTINE desaloca_k

!------------------------------------------------------------------------------------------------------------------
  SUBROUTINE rk4(func,y,t,ti,tf,n)
    IMPLICIT NONE
    REAL(pr),DIMENSION(:,:),INTENT(INOUT)    :: y                ! Matriz que serão armazenadas as soluções das variáveis explicitas 
    REAL(pr),DIMENSION(:),INTENT(OUT)        :: t                ! Vetor que serão armazenadas as soluções da variável implicita
    REAL(ps),INTENT(IN)                      :: ti,tf            ! Tempos incial e final de simulação
    REAL(pr)                                 :: h                ! Passo do método RK4
    INTEGER(ip3)                             :: i                ! Loop
    INTEGER(ip3),INTENT(IN)                  :: n                ! Número de iterações do método RK4
!------------------------------------------------------------------------------------------------------------------
INTERFACE
   FUNCTION func(z)
    USE precisao
    IMPLICIT NONE
     REAL(pr),DIMENSION(:),INTENT(IN)  :: z
     REAL(pr),DIMENSION(size(z))       :: func
   END FUNCTION func
END INTERFACE
    k(:,:)= 0.0_pr
!------------------------------------------------------------------------------------------------------------------
    h=(tf-ti)/(REAL(n-1))                                                ! Definindo o passo do método RK4
!------------------------------------------------------------------------------------------------------------------
    DO i=1,n-1
       t(i+1) = t(1)+h*i                                                 ! Definindo o passo temporal do método RK4                                                       
       k(1,:) = h*func(y(i,:))
       k(2,:) = h*func(y(i,:)+0.5*k(1,:))            
       k(3,:) = h*func(y(i,:)+0.5*k(2,:))            
       k(4,:) = h*func(y(i,:)+k(3,:))
       y(i+1,:) = y(i,:) + (k(1,:) + 2*k(2,:) + 2*k(3,:) + k(4,:))/6
     END DO
!------------------------------------------------------------------------------------------------------------------
  END SUBROUTINE rk4
!------------------------------------------------------------------------------------------------------------------
!  SUBROUTINE rkf45(func,y,t,ti,tf,h0,hf, tol, n)
!    IMPLICIT NONE
!    REAL(pr),DIMENSION(:,:),INTENT(INOUT) :: y                ! Matriz que serão armazenadas as soluções das variáveis explicitas 
!    REAL(pr),DIMENSION(:),INTENT(OUT)     :: t                ! Vetor que serão armazenadas as soluções da variável implicita
!    REAL(ps),INTENT(IN)                   :: ti,tf            ! Tempos incial e final de simulação
!    REAL(ps),INTENT(IN)                   :: h0, hf, tol      ! passo minimo e máximo
!    REAL(pr)                                 :: h                ! Passo do método RK4
!    INTEGER(ip3)                             :: i                ! Loop
!    INTEGER(ip3),INTENT(IN)                  :: n                ! Número de iterações do método RK4
!------------------------------------------------------------------------------------------------------------------
!INTERFACE
!   FUNCTION func(z)
!    USE precisao
!    IMPLICIT NONE
!     REAL(pr),DIMENSION(:),INTENT(IN)  :: z
!     REAL(pr),DIMENSION(size(z))       :: func
!   END FUNCTION func
!END INTERFACE
!    k(:,:)= 0.0_pr
!------------------------------------------------------------------------------------------------------------------
!    h=(tf-ti)/(REAL(n-1))                                                ! Definindo o passo do método RK4
!------------------------------------------------------------------------------------------------------------------
!
!def rkf45(fxy, a, b, y0, hmin, hmax, nx, tol):
!    function [XY] = RungeKuttaFehlberg(a, b, y0, hmin, hmax, tol, cor)
!    Equações diferenciais ordinárias de segunda ordem
!    Método de Runge-Kutta-Felberg
!    Parâmetros de Entradas :
!           [a,b] Intervalo de integração
!           hmin e hmax  Passo minímo e máximo da integração
!            y0    Condição inicial em x0
!            Tol   tolerância
!    Saída : Matriz XYH, na primeira coluna os valores de x
!                        na segunda coluna os valores de y (a solução)
!                        na terceira coluna o paso usado
!    Aqui y  : é a função
!    import numpy as np
!    # Vamos definir as constantes do método
!    # Os coeficientes dos k_i
!    #=======================================================
!    a21 = 1.0/4.0
!    a31 = 3.0/32.0  
!    a32 = 9.0/32.0
!    a41 = 1932.0/2197.0
!    a42 = -7200.0/2197.0
!    a43 = 7296.0/2197.0
!    a51 = 439.0/216.0
!    a52 = -8.0 
!    a53 = 3680.0/513.0 
!    a54 = -845.0/4104.0
!    a61 = -8.0/27.0
!    a62 = 2.0
!    a63 = -3544.0/2565.0
!    a64 = 1859.0/4104.0
!    a65 = -11.0/40.0
!    #=======================================================
!    # Os coeficientes dos h_i
!    c2 = 1.0/4.0
!    c3 = 3.0/8.0
!    c4 = 12.0/13.0
!    c5 = 1.0
!    c6 = 0.50
!    #=======================================================
!    # Os coeficientes dos R_i
!    e1 = 1.0/360.0
!    e3 = -128.0/4275.0
!    e4 = -2197.0/75240.0
!    e5 = 1.0/50.0
!    e6 = 2.0/55.0
!    #=======================================================
!    # Os coeficientes dos x_i para o novo passo
!    p1 = 25.0/216.0
!    p3 = 1408.0/2565.0
!    p4 = 2197.0/4104.0
!    p5 = -1.0/5.0
!    #=======================================================
!    # Vamos inicializar os arrays que serão retornados
!    t(1) = t0
!    h = hmax
!    ok = 1
!    i=0
!    loop: do 
!        k1 = h*fxy(x,y)
!        xx  = x + c2*h
!        yy  = y + a21*k1
!        k2 = h*fxy(xx,yy)
!        xx  = x + c3*h
!        yy  = y + a31*k1 + a32*k2
!        k3 = h*fxy(xx,yy)
!        xx  = x + c4*h
!        yy  = y + a41*k1 + a42*k2 + a43*k3
!        k4 = h*fxy(xx,yy)
!        xx  = x + h
!        yy  = y + a51*k1 + a52*k2 + a53*k3 + a54*k4
!        k5 = h*fxy(xx,yy)
!        xx  = x + c6*h
!        yy  = y + a61*k1 + a62*k2 + a63*k3 + a64*k4 + a65*k5
!        k6 = h*fxy(xx,yy)
!        r = abs(e1*k1 + e3*k3 + e4*k4 + e5*k5 + e6*k6)/h
!        if (r <= tol):
!            # Aproximacao aceita
!            x = x+h
!            y = y + p1*k1 + p3*k3 + p4*k4 + p5*k5
!            i = i + 1
!            X = np.append( X, [x], 0 )
!            Y = np.append( Y, [y], 0 )            
!            #print('x = ', x, 'y = ',y)
!#            if (ok == 1):
!#                ok = 0
!        # Evitando underflow
!        if (r > 1.0e-20): 
!            delta = 0.84*(tol/r)**0.25
!        else:
!            delta = 10.0
!        # Cálculo do novo h
!        if (delta <= 0.1):
!            h = 0.1*h
!        else:
!            if (delta >= 4):
!                h = 4.0*h
!            else:
!                h = delta*h
!        # Estimando Hmax
!        #print("h = ",h)
!        if (h > hmax):
!            h = hmax
!        # Estimando Hmin
!        if (h < hmin):
!            ok = 0
!            #print("hmin = ",h)
!        else:
!            if (x+h > b):
!                if ( abs(b-x) < tol):
!                    x = b
!                else:
!                    h = b-x
!                ok = 1
!                #print("hmin = ",h, "Flag = ", ok, "hmax = ", hmax)
!     if ( (t(i) < tf) .and. (ok == 1) ) then
!        cycle loop
!     else
!        exit loop
!     endif 
!     end do loop
!#
!    if (ok == 0) then
!        print *, 'O valor minimo de h foi ultrapassado'
!    endif
!        
!  END SUBROUTINE rkfk5
!
END MODULE Met_edos
