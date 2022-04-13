MODULE precisao
!------------------------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! # Definindo precisão para os possíveis tipos de Reais
  INTEGER, PARAMETER :: ps  = SELECTED_REAL_KIND(5,30)                     ! Precisão simples
  INTEGER, PARAMETER :: pd  = SELECTED_REAL_KIND(8,100)                    ! Precisão dupla
  INTEGER, PARAMETER :: pq  = SELECTED_REAL_KIND(18,400)                   ! Precisão Quadrupla
!------------------------------------------------------------------------------------------------------------------
  INTEGER, PARAMETER :: pr  = pd                                           ! Definindo precisão padrão dos reais
!------------------------------------------------------------------------------------------------------------------
! # Definindo precisão para os possíveis tipos de inteiros
  INTEGER, PARAMETER :: ip1 = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: ip2 = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: ip3 = SELECTED_INT_KIND(8)
  INTEGER, PARAMETER :: ip4 = SELECTED_INT_KIND(10)
  INTEGER, PARAMETER :: ip5 = SELECTED_INT_KIND(20)
!------------------------------------------------------------------------------------------------------------------
END MODULE precisao
