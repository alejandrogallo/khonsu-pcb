LIBRARIES = Keebio-Parts.pretty keyswitches.pretty keyboard_parts.pretty MX_Alps_Hybrid

all: $(LIBRARIES)

clean:
	rm -rf $(LIBRARIES)

CLONE = git clone --depth=1

Keebio-Parts.pretty:
	$(CLONE) https://github.com/keebio/Keebio-Parts.pretty
	rm -vrf $@/.git*

keyswitches.pretty:
	$(CLONE) https://github.com/daprice/keyswitches.pretty
	rm -vrf $@/.git*

keyboard_parts.pretty:
	$(CLONE) https://github.com/tmk/keyboard_parts.pretty
	rm -vrf $@/.git*

MX_Alps_Hybrid:
	$(CLONE) https://github.com/ai03-2725/MX_Alps_Hybrid
	rm -vrf $@/.git*
