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
    For instance, read the help message.
    ```
    stack exec simulate-trees -- -h

    --

    Simulate reconstructed trees

    Usage: simulate-trees [-t|--nTrees INT] [-n|--nSamples INT] [-H|--height DOUBLE]
                          [-l|--lambda DOUBLE] [-m|--mu DOUBLE] [-v|--verbosity]
                          [-q|--quiet]
      Simulate reconstructed trees using the point process.

    Available options:
      -h,--help                Show this help text
      -t,--nTrees INT          Number of trees (default: 10)
      -n,--nSamples INT        Number of samples per tree (default: 5)
      -H,--height DOUBLE       Tree height (default: 1.0)
      -l,--lambda DOUBLE       Birth rate lambda (default: 1.0)
      -m,--mu DOUBLE           Death rate mu (default: 0.9)
      -v,--verbosity           Verbosity
      -q,--quiet               Be quiet
    ```
    Or do a test simulation.
    ```
    stack exec simulate-trees
    
    --
    
    Reconstructed trees simulator version 0.1.0.0.
    Command line: simulate-trees 

    -- Arguments
    Number of simulated trees: 10
    Number of species per tree: 5
    Height of trees: 1.0
    Birth rate: 1.0
    Death rate: 0.9
    Verbosity: False
    Quiet: False

    -- Simulation
    (0:0.3774855500765698,((1:2.0154241024089484e-2,2:2.0154241024089484e-2)0:0.2194498304554361,(3:0.18875196496260377,4:0.18875196496260377)0:5.085210651692179e-2)0:0.13788147859704425)0:0.6225144499234302;
    ((0:0.5177388882792351,1:0.5177388882792351)0:0.17579988502229682,((2:2.4071319480028328e-3,3:2.4071319480028328e-3)0:0.5087754559038138,4:0.5111825878518166)0:0.18235618544971532)0:0.3064612266984681;
    (0:0.5844174857147952,(1:0.5698359606751093,(2:0.4090026429825453,(3:9.963595012789103e-2,4:9.963595012789103e-2)0:0.30936669285465423)0:0.16083331769256404)0:1.4581525039685905e-2)0:0.41558251428520476;
    (0:0.45748809145221386,((1:3.1144304318300375e-3,2:3.1144304318300375e-3)0:0.27978666828928417,(3:5.2227265132491034e-2,4:5.2227265132491034e-2)0:0.23067383358862315)0:0.17458699273109968)0:0.5425119085477861;
    (((0:6.735311917877203e-2,1:6.735311917877203e-2)0:3.0393396323536895e-2,(2:7.177846060176821e-2,3:7.177846060176821e-2)0:2.5968054900540716e-2)0:0.13914199440844016,4:0.23688850991074908)0:0.7631114900892509;
    ((0:0.2100809945775664,(1:0.15777006486332162,2:0.15777006486332162)0:5.231092971424478e-2)0:3.0252921468290034e-2,(3:0.1501753882670414,4:0.1501753882670414)0:9.015852777881503e-2)0:0.7596660839541436;
    ((((0:3.2439737590665e-2,1:3.2439737590665e-2)0:0.36980767383515173,2:0.4022474114258167)0:6.58267022258962e-2,3:0.4680741136517129)0:0.40516374800937494,4:0.8732378616610879)0:0.12676213833891214;
    ((((0:0.11196444537163554,1:0.11196444537163554)0:5.056354677189623e-3,2:0.11702080004882516)0:0.28450453494869715,3:0.4015253349975223)0:2.0093485295044233e-2,4:0.42161882029256653)0:0.5783811797074334;
    (0:0.9803439102203049,(((1:4.690857183501333e-2,2:4.690857183501333e-2)0:3.103274247509455e-2,3:7.794131431010788e-2)0:0.5342528598722517,4:0.6121941741823596)0:0.3681497360379453)0:1.965608977969513e-2;
    ((0:0.3170383445505747,1:0.3170383445505747)0:0.40209444148245527,((2:0.1770647724894815,3:0.1770647724894815)0:0.4894164144749219,4:0.6664811869644034)0:5.26515990686266e-2)0:0.28086721396697;
    ```
        
5. Install (take care that the binary is installed into your `PATH`).

        stack install geneclocks:\exe:\simulate-trees
        
