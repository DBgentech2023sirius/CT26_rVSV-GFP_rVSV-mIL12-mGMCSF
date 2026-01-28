# Modeling CT26 tumor treatment with VSV-GFP and VSV-mIL12-mGMCSF in mice

Model is able to predict CT26 tumor growth curves in mice treated with placebo, rVSV-GFP, and rVSV-mIL12-mGMCSF

![Final model](https://github.com/DBgentech2023sirius/VSV-mIL12-mGMCSF/blob/main/Images/Model%20diagram.png?raw=true)

The model considers 5 types of cells:

<ol>
    <li>Uninfected tumor cells (Tu)</li>
    <li>VSV-infected tumor cells (Ti)</li>
    <li>M1 macrophages (M1)</li>
    <li>M2 macrophages (M2)</li>
    <li>CD8+ T-cells (Teff)</li>
</ol>

and 2 therapeutic agents:

<ol>
    <li>VSV</li>
    <li>mIL12-mGMCSF fusion protein</li>
</ol>

The following treatment regimens were described in the model:

<ol>
    <li>Three intra-tumor injections of 100 µl PBS on days 11, 14, and 17 after tumor inoculation</li>
    <li>Three intra-tumor injections of VSV encoding the GFP at a dose of 2*10<sup>6</sup> in a volume of 100 μl on days 11, 14, and 17 after tumor inoculation</li>
    <li>Three intra-tumor injections of VSV encoding the fusion protein of murine IL-12 (mIL12) and murine GMCSF (mGMCSF) at a dose of 2*10<sup>6</sup> in a volume of 100 μl on days 11, 14, and 17 after tumor inoculation</li>
</ol>

<br>

<p align="center">
  <a href="https://sirius-web.org/bioumlweb/#de=data/Collaboration%20(git)/CT26%20rVSV-GFP%20rVSV-mIL12-mGMCSF/Data/Diagrams/Ryapolova%20and%20Babaev%202026%20-%20CT26%20tumor%20treatment%20with%20rVSV-GFP%20and%20rVSV-mIL12-mGMCSF">
    <img src="https://github.com/DBgentech2023sirius/VSV-mIL12-mGMCSF/blob/main/Images/Test_model_in_the_BioUML_button.png?raw=true" width="300" alt="Test model in the BioUML" />
  </a>
</p>

You can reproduce all plots used in our work in two ways (whichever is more convenient for you):

<ol>
    <li>Clone the entire repository to your local computer and run Jupyter notebook "Create_plots.ipynb"</li></li>
    <li>Run Jupyter notebook "Create_plots.ipynb" directly in Google Colab <a href="https://colab.research.google.com/drive/1po2Ts9VNnKtKzRMtrIbYwcjqBWyn2wKv?usp=drive_link" target="_blank"> <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab" style="height: 30px;"></a></li>
</ol>
