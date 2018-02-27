# Geneclocks project

For now, only simulation of reconstructed trees is supported.

# Installation

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).

2. Clone this project.

        git clone https://github.com/dschrempf/geneclocks

3. Build.

        cd geneclocks
        stack build

4. Run `simulate-trees`.

        stack exec simulate-trees -- -h
        
5. Or install (take care that the binary is installed into your `PATH`).

        stack install geneclocks:\exe:\simulate-trees
