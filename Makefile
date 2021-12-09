advanced-example-%: %/
	ghcid $</advanced.hs --setup ":set args $</input.example.txt" --test main

advanced-%: %/
	ghcid $</advanced.hs --setup ":set args $</input.txt" --test main

simple-example-%: %/
	ghcid $</simple.hs --setup ":set args $</input.example.txt" --test main

simple-%: %/
	ghcid $</simple.hs --setup ":set args $</input.txt" --test main