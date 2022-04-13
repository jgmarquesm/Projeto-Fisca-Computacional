PROGRAM penduloduplo
!--------------------------------------------------------------------------------------------------
! # Usando os módulos
  USE precisao
  USE leitura
  USE p2s
  USE Met_edos
!--------------------------------------------------------------------------------------------------
! # Declaração de variáveis
IMPLICIT NONE
  REAL(pr),DIMENSION(:,:),ALLOCATABLE :: y                  ! Vetor que serão escritas as soluções 
  REAL(pr),DIMENSION(:),ALLOCATABLE   :: t                  ! Tempo
  INTEGER(ip3)         :: i,s,AllocateStatus
  INTEGER(ip1)         :: b
  CHARACTER(LEN=20)    :: caso
  CHARACTER(LEN=100)   :: arqv
  CHARACTER(LEN=100)   :: file2,pgraf,pgraf2
  LOGICAL              :: ecaso
!--------------------------------------------------------------------------------------------------
! # Selecionando o sistema que vamos resolver
PRINT*, 'Escolha o sistema que será resolvido:'
PRINT*, '1 - Pêndulo duplo convencional'
PRINT*, '2 - Pêndulo duplo com mola1'
PRINT*, '3 - Pêndulo duplo com mola2'
PRINT*, '4 - Pêndulo duplo com mola1 e mola2'
PRINT*, ' '
PRINT*, 'Entre com o número referente ao sistema escolhido: '
PRINT*, ' '
READ(*,*) b
PRINT*, ' '
!--------------------------------------------------------------------------------------------------
! # Lendo arquivo de entrada
DO 
  WRITE(*,*) "Entre com o nome do arquivo de entrada: "
   PRINT*, ' '
   READ(*,*,ERR=10,IOSTAT=i) caso
10  IF (i==0) THEN 
     caso=ADJUSTL(TRIM(caso))//'.inp'
      INQUIRE (FILE=caso,EXIST=ecaso)                           ! Verificando a existência do arquivo
     IF (.NOT.ecaso) THEN
       WRITE (*,*) "Arquivo não encontrado: ", caso
        CYCLE                                                   ! Volta ao inicio do loop
      ELSE 
        EXIT                                                    ! Sai do loop
     END IF
      ELSE 
       WRITE (*,*) 'Falha. Identificação do erro: #', i
        CYCLE                                                   ! Volta ao inicio do loop
    END IF        
END DO 
!--------------------------------------------------------------------------------------------------
CALL lercaso(b,caso)
ALLOCATE(t(1:n),stat=AllocateStatus)
      IF(AllocateStatus /= 0) PRINT*, '===> Erro na alocação do vetor que armazena os tempos <==='
   ALLOCATE(y(1:n,1:2*gl),stat=AllocateStatus)
      IF(AllocateStatus /= 0) PRINT*, '===> Erro na alocação da matriz que armazena as soluções <==='
!--------------------------------------------------------------------------------------------------
SELECT CASE (b)
CASE (1)
!--------------------------------------------------------------------------------------------------    
! # Condições iniciais
  y(1,1) = o10
  y(1,2) = o20
  y(1,3) = w10 
  y(1,4) = w20
  t(1)   = t0
!--------------------------------------------------------------------------------------------------
! # Resolvendo as esquções diferenciais
CALL alocar_k(gl)
CALL rk4(f1,y,t,t0,tf,n)
!--------------------------------------------------------------------------------------------------
! # Gerando padrão de nome do arquivo de saída 
  arqv  = '_tf_'//TRIM(ADJUSTL(r2c(tf,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_L1_'//TRIM(ADJUSTL(r2c(l1,1)))  
  arqv  = TRIM(ADJUSTL(arqv))//'_L2_'//TRIM(ADJUSTL(r2c(l2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M1_'//TRIM(ADJUSTL(r2c(m1,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M2_'//TRIM(ADJUSTL(r2c(m2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ1_'//TRIM(ADJUSTL(r2c(o10,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ2_'//TRIM(ADJUSTL(r2c(o20,1)))//'.out'   
  file2 = TRIM(ADJUSTL(nome))//TRIM(ADJUSTL(arqv))
!--------------------------------------------------------------------------------------------------
! # Escrevendo os arquivos de saída    
OPEN(UNIT=21,FILE=file2,FORM='FORMATTED',STATUS='UNKNOWN')
        !# y(s,1) = o1(s); y(s,2) = o2(s); y(s,3) = w1(s); y(s,4) = w2(s)
     DO s=1,n
          WRITE(21,*) t(s),y(s,1),y(s,2)
     END DO
CLOSE(UNIT=21,STATUS='KEEP')
!--------------------------------------------------------------------------------------------------
! # Plotando os gráficos usando o gnuplot
  pgraf = 'os_vs_t.txt'
OPEN(UNIT=22,FILE=pgraf,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(22,*) 'set xlabel "t[s]"'
  WRITE(22,*) 'set ylabel "o1(t), o2(t)[rad]"'
  WRITE(22,*) 'set title "o1(t), o2(t) x tempo"'
  WRITE(22,*) 'set grid'
  WRITE(22,*) 'set style data lines'
  WRITE(22,*) 'plot "'//trim(file2)//'" u 1:2 w l title "o1(t)", "'//trim(file2)//'" u 1:3 w l title "o2(t)"'
CLOSE(UNIT=22,STATUS='KEEP')      
CALL execute_command_line("gnuplot -persist os_vs_t.txt")
CALL desaloca_k(i)
!--------------------------------------------------------------------------------------------------
CASE (2) 
!--------------------------------------------------------------------------------------------------   
! # Condições iniciais
  y(1,1) = o10
  y(1,2) = o20
  y(1,3) = l10 
  y(1,4) = w10
  y(1,5) = w20 
  y(1,6) = v10
  t(1)   = t0
!--------------------------------------------------------------------------------------------------
! # Resolvendo as esquções diferenciais
CALL alocar_k(gl)
CALL rk4(f2,y,t,t0,tf,n)
!--------------------------------------------------------------------------------------------------
! # Gerando padrão de nome do arquivo de saída 
  arqv  = '_tf_'//TRIM(ADJUSTL(r2c(tf,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_L10_'//TRIM(ADJUSTL(r2c(l10,1)))  
  arqv  = TRIM(ADJUSTL(arqv))//'_L2_'//TRIM(ADJUSTL(r2c(l2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M1_'//TRIM(ADJUSTL(r2c(m1,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M2_'//TRIM(ADJUSTL(r2c(m2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ1_'//TRIM(ADJUSTL(r2c(o10,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ2_'//TRIM(ADJUSTL(r2c(o20,1)))//'.out'   
  file2 = TRIM(ADJUSTL(nome))//TRIM(ADJUSTL(arqv))
!--------------------------------------------------------------------------------------------------
! # Escrevendo os arquivos de saída    
OPEN(UNIT=21,FILE=file2,FORM='FORMATTED',STATUS='UNKNOWN')
        !# y(s,1) = o1(s); y(s,2) = o2(s); y(s,3) = w1(s); y(s,4) = w1(s); y(s,5) = w2(s); y(s,6) = l1(s)
     DO s=1,n
          WRITE(21,*) t(s),y(s,1),y(s,2),y(s,3)
     END DO
CLOSE(UNIT=21,STATUS='KEEP')
!--------------------------------------------------------------------------------------------------
! # Plotando os gráficos usando o gnuplot
  pgraf = 'os_vs_t.txt'
OPEN(UNIT=22,FILE=pgraf,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(22,*) 'set xlabel "t[s]"'
  WRITE(22,*) 'set ylabel "o1(t), o2(t)[rad]"'
  WRITE(22,*) 'set title "o1(t), o2(t) x tempo"'
  WRITE(22,*) 'set grid'
  WRITE(22,*) 'set style data lines'
  WRITE(22,*) 'plot "'//trim(file2)//'" u 1:2 w l title "o1(t)", "'//trim(file2)//'" u 1:3 w l title "o2(t)"'
CLOSE(UNIT=22,STATUS='KEEP')      
  pgraf2 = 'r1_vs_t.txt'
OPEN(UNIT=23,FILE=pgraf2,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(23,*) 'set xlabel "t[s]"'
  WRITE(23,*) 'set ylabel "r1(t)[m]"'
  WRITE(23,*) 'set title "r1(t) x tempo"'
  WRITE(23,*) 'set grid'
  WRITE(23,*) 'set style data lines'
  WRITE(23,*) 'plot "'//trim(file2)//'" u 1:4 w l title "r1(t)"'
CLOSE(UNIT=23,STATUS='KEEP') 
CALL execute_command_line("gnuplot -persist os_vs_t.txt")
CALL execute_command_line("gnuplot -persist r1_vs_t.txt")
CALL desaloca_k(i)
!--------------------------------------------------------------------------------------------------
CASE (3)
!--------------------------------------------------------------------------------------------------
! # Condições iniciais
  y(1,1) = o10
  y(1,2) = o20
  y(1,3) = l20 
  y(1,4) = w10
  y(1,5) = w20 
  y(1,6) = v20
  t(1)   = t0
!--------------------------------------------------------------------------------------------------
! # Resolvendo as esquções diferenciais
CALL alocar_k(gl)
CALL rk4(f3,y,t,t0,tf,n)
!--------------------------------------------------------------------------------------------------
! # Gerando padrão de nome do arquivo de saída 
  arqv  = '_tf_'//TRIM(ADJUSTL(r2c(tf,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_L1_'//TRIM(ADJUSTL(r2c(l1,1)))  
  arqv  = TRIM(ADJUSTL(arqv))//'_L20_'//TRIM(ADJUSTL(r2c(l20,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M1_'//TRIM(ADJUSTL(r2c(m1,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M2_'//TRIM(ADJUSTL(r2c(m2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ1_'//TRIM(ADJUSTL(r2c(o10,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ2_'//TRIM(ADJUSTL(r2c(o20,1)))//'.out'   
  file2 = TRIM(ADJUSTL(nome))//TRIM(ADJUSTL(arqv))
!--------------------------------------------------------------------------------------------------
! # Escrevendo os arquivos de saída    
OPEN(UNIT=21,FILE=file2,FORM='FORMATTED',STATUS='UNKNOWN')
        !# y(s,1) = o1(s); y(s,2) = o2(s); y(s,3) = l2(s); y(s,4) = w1(s); y(s,5) = w2(s); y(s,6) = v2(s)
     DO s=1,n
          WRITE(21,*) t(s),y(s,1),y(s,2),y(s,3)
     END DO
CLOSE(UNIT=21,STATUS='KEEP')
!--------------------------------------------------------------------------------------------------
! # Plotando os gráficos usando o gnuplot
  pgraf = 'os_vs_t.txt'
OPEN(UNIT=22,FILE=pgraf,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(22,*) 'set xlabel "t[s]"'
  WRITE(22,*) 'set ylabel "o1(t), o2(t)[rad]"'
  WRITE(22,*) 'set title "o1(t), o2(t) x tempo"'
  WRITE(22,*) 'set grid'
  WRITE(22,*) 'set style data lines'
  WRITE(22,*) 'plot "'//trim(file2)//'" u 1:2 w l title "o1(t)", "'//trim(file2)//'" u 1:3 w l title "o2(t)"'
CLOSE(UNIT=22,STATUS='KEEP')      
  pgraf2 = 'r2_vs_t.txt'
OPEN(UNIT=23,FILE=pgraf2,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(23,*) 'set xlabel "t[s]"'
  WRITE(23,*) 'set ylabel "r2(t)[m]"'
  WRITE(23,*) 'set title "r2(t) x tempo"'
  WRITE(23,*) 'set grid'
  WRITE(23,*) 'set style data lines'
  WRITE(23,*) 'plot "'//trim(file2)//'" u 1:4 w l title "r2(t)"'
CLOSE(UNIT=23,STATUS='KEEP') 
CALL execute_command_line("gnuplot -persist os_vs_t.txt")
CALL execute_command_line("gnuplot -persist r2_vs_t.txt")
CALL desaloca_k(i)
!--------------------------------------------------------------------------------------------------
CASE(4)
!--------------------------------------------------------------------------------------------------
! # Condições iniciais
  y(1,1) = o10
  y(1,2) = o20
  y(1,3) = l10 
  y(1,4) = l20
  y(1,1) = w10
  y(1,2) = w20
  y(1,3) = v10 
  y(1,4) = v20
  t(1)   = t0
!--------------------------------------------------------------------------------------------------
! # Resolvendo as esquções diferenciais
CALL alocar_k(gl)
CALL rk4(f4,y,t,t0,tf,n)
!--------------------------------------------------------------------------------------------------
! # Gerando padrão de nome do arquivo de saída 
  arqv  = '_tf_'//TRIM(ADJUSTL(r2c(tf,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_L10_'//TRIM(ADJUSTL(r2c(l10,1)))  
  arqv  = TRIM(ADJUSTL(arqv))//'_L20_'//TRIM(ADJUSTL(r2c(l20,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M1_'//TRIM(ADJUSTL(r2c(m1,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_M2_'//TRIM(ADJUSTL(r2c(m2,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ1_'//TRIM(ADJUSTL(r2c(o10,1)))
  arqv  = TRIM(ADJUSTL(arqv))//'_θ2_'//TRIM(ADJUSTL(r2c(o20,1)))//'.out'   
  file2 = TRIM(ADJUSTL(nome))//TRIM(ADJUSTL(arqv))
!--------------------------------------------------------------------------------------------------
! # Escrevendo os arquivos de saída    
OPEN(UNIT=21,FILE=file2,FORM='FORMATTED',STATUS='UNKNOWN')
        !# y(s,1) = o1(s); y(s,2) = o2(s); y(s,3) = l1(s); y(s,4) = l2(s); y(s,5) = w1(s); y(s,6) = w2(s); y(s,7) = r1(s); y(s,8) = r2(s)
     DO s=1,n
          WRITE(21,*) t(s),y(s,1),y(s,2),y(s,3),y(s,4)
     END DO
CLOSE(UNIT=21,STATUS='KEEP')
!--------------------------------------------------------------------------------------------------
! # Plotando os gráficos usando o gnuplot
  pgraf = 'os_vs_t.txt'
OPEN(UNIT=22,FILE=pgraf,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(22,*) 'set xlabel "t[s]"'
  WRITE(22,*) 'set ylabel "o1(t), o2(t)[rad]"'
  WRITE(22,*) 'set title "o1(t), o2(t) x tempo"'
  WRITE(22,*) 'set grid'
  WRITE(22,*) 'set style data lines'
  WRITE(22,*) 'plot "'//trim(file2)//'" u 1:2 w l title "o1(t)", "'//trim(file2)//'" u 1:3 w l title "o2(t)"'
CLOSE(UNIT=22,STATUS='KEEP')
  pgraf2 = 'rs_vs_t.txt'
OPEN(UNIT=23,FILE=pgraf2,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(23,*) 'set xlabel "t[s]"'
  WRITE(23,*) 'set ylabel "r1(t), r2(t)[m]"'
  WRITE(23,*) 'set title "r1(t), r2(t) x tempo"'
  WRITE(23,*) 'set grid'
  WRITE(23,*) 'set style data lines'
  WRITE(23,*) 'plot "'//trim(file2)//'" u 1:4 w l title "r1(t)", "'//trim(file2)//'" u 1:5 w l title "r2(t)"'
CLOSE (UNIT=23,STATUS='KEEP')
CALL execute_command_line("gnuplot -persist os_vs_t.txt")
CALL execute_command_line("gnuplot -persist rs_vs_t.txt")
CALL desaloca_k(i)
!--------------------------------------------------------------------------------------------------
CASE DEFAULT
    PRINT*, 'Você não selecionou nenhuma das possívies opções acima'        
!--------------------------------------------------------------------------------------------------
END SELECT
PRINT*, ' '
PRINT*, 'O problema resolvido foi referente ao sistema: ', b
PRINT*, 'Certfique-se que fez corretamente a escolha do caso.'
PRINT*, ' '
PRINT*, 'Programa executado e finalizado.'
!--------------------------------------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------------------------------------
  FUNCTION r2c(p,nd)
    USE precisao
    IMPLICIT NONE
    REAL(ps),INTENT (IN)     :: p
    INTEGER(ip3),INTENT(IN)  :: nd
    INTEGER(ip3)             :: ni
    CHARACTER (len=90)       :: fmt0,aux
    CHARACTER (len=100)      :: r2c
    IF (p<0) THEN 
     ni=INT(LOG10(ABS(p)))+2
    ELSE IF (p<10E-10) THEN
     ni=1
    ELSE
     ni=INT(LOG10(p))+1
    END IF
    WRITE(fmt0,'(i15)') ni+nd+1                                
    WRITE(aux,'(i15)') nd
    fmt0 = '(F'//TRIM(ADJUSTL(fmt0))//'.'//TRIM(ADJUSTL(aux))//')'
    WRITE(r2c, fmt0) p  
  END FUNCTION r2c
!--------------------------------------------------------------------------------------------------
END PROGRAM penduloduplo
