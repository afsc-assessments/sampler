EXEC = sam
ARGS = -nox 
DIST = ../../../src/

ifdef ComSpec
    RM=del /F /Q
else
    RM=rm -rf
endif

all: $(EXEC)
	./$(EXEC) $(ARGS)

$(EXEC): $(DIST)$(EXEC).tpl
	ln -sf $(DIST)$(EXEC) $@

$(DIST)$(EXEC).tpl:
	$(MAKE) --directory=../../../src

run: $(EXEC)
	./$(EXEC) $(ARGS)

clean:
	@$(RM) -f $(EXEC)
	@$(RM) -f admodel.*
	@$(RM) -f $(EXEC)  $(EXEC).[brces]* $(EXEC).*[0123456789] *.rpt *.log variance

