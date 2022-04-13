# Início do  makefile

PROG =	p2

SRCS =	precisao.f95 leitura.f95 p2s.f95 Met_edos.f95 main_p2.f95

OBJS =	precisao.o leitura.o p2s.o Met_edos.o main_p2.o 

F90 = gfortran

F90FLAGS = -O3 -Wall -Waliasing -pedantic -Warray-bounds -Wsurprising -Wunderflow -fcheck=all -std=f2008

# Opções para link
LDFLAGS = -s

# Alvo padrão: all
all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS)


# Limpando tudo
clean:
	rm -f $(PROG) $(OBJS) *.mod
    
.SUFFIXES: $(SUFFIXES) .f95

.f95.o:
	$(F90) $(F90FLAGS) -c $<
    
# As dependencias geradas pelo makedepf90
precisao.o : precisao.f95 
leitura.o : leitura.f95 precisao.o 
p2s.o : p2s.f95 precisao.o leitura.o
Met_edos.o : Met_edos.f95 precisao.o
main_p2.o : main_p2.f95 precisao.o leitura.o p2s.o Met_edos.o

# Fim do makefile
