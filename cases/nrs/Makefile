# make yr=XXXX will run for sampler for that year (assumin files exist)
# make yrs yr1=XXXX yr2=YYYY will run for sampler for years between XXXX and YYYY (assumin files exist)
EXEC = sam
DIST = ../../src/
ARGS = -ind  
ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

#all: sam 
#action:
#sam: sam.tpl 
	#@admb -f sam.tpl 

all: mpd
$(EXEC): $(DIST)$(EXEC).tpl
	ln -sf $(DIST)$(EXEC) $@
	ln -sf $(DIST)$(EXEC).tpl $@.tpl

$(DIST)$(EXEC).tpl:
	$(MAKE) --directory=../../src

mpd: $(EXEC)
		./$(EXEC) $(ARGS) data/sam_$(yr).dat

yrs:
	number=$(yr1) ; while [[ $$number -le $(yr2) ]] ; do \
   	echo data/sam_$$number\.dat ; \
   	((number = number + 1)) ; \
	done

clean:
	@$(RM) $(EXEC)
