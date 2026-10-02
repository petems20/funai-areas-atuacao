"""Exporta layouts de impressao especificos de um projeto QGIS (.qgz) para PNG e PDF.

Roda em modo headless via python-qgis.bat (ambiente PyQGIS do OSGeo4W). Apenas LE o
projeto (QgsProject.read) e exporta os layouts pedidos -- o projeto original nunca e
salvo/alterado.

Uso:
    python-qgis.bat exportar_layouts_qgis.py <projeto.qgz> <dir_saida> <layout_1> [<layout_2> ...]

Imprime uma linha "ARQUIVO_EXPORTADO: <caminho>" para cada PNG/PDF gerado (inclusive
paginas extras de layouts com mais de uma pagina, que o QGIS grava como
"<nome>_2.png", "<nome>_3.png" etc.), para que o lado R (via system2()) consiga coletar
a lista completa de arquivos produzidos.
"""

import glob
import os
import re
import sys
import unicodedata

from qgis.core import QgsApplication, QgsLayoutExporter, QgsProject


def slugify(nome):
    """Remove acentos e caracteres invalidos em nome de arquivo, preservando espacos."""
    nome_sem_acento = unicodedata.normalize("NFKD", nome).encode("ascii", "ignore").decode("ascii")
    return re.sub(r'[\\/:*?"<>|]', "_", nome_sem_acento).strip()


def main():
    if len(sys.argv) < 4:
        print("Uso: exportar_layouts_qgis.py <projeto.qgz> <dir_saida> <layout_1> [<layout_2> ...]", file=sys.stderr)
        sys.exit(1)

    caminho_projeto, dir_saida = sys.argv[1], sys.argv[2]
    nomes_pedidos = sys.argv[3:]

    qgs = QgsApplication([], False)
    qgs.initQgis()

    try:
        project = QgsProject.instance()
        if not project.read(caminho_projeto):
            print(f"ERRO: falha ao ler o projeto QGIS em {caminho_projeto}", file=sys.stderr)
            sys.exit(1)

        lm = project.layoutManager()

        for nome_layout in nomes_pedidos:
            layout = lm.layoutByName(nome_layout)
            if layout is None:
                print(f"ERRO: layout '{nome_layout}' nao encontrado no projeto.", file=sys.stderr)
                sys.exit(1)

            nome_arquivo = slugify(layout.name())
            exporter = QgsLayoutExporter(layout)

            # PNG: layouts com mais de uma pagina geram "<nome>.png", "<nome>_2.png", ...
            # (comportamento padrao do QGIS, PNG nao suporta multiplas paginas num arquivo).
            caminho_png = f"{dir_saida}/{nome_arquivo}.png"
            resultado_png = exporter.exportToImage(caminho_png, QgsLayoutExporter.ImageExportSettings())
            if resultado_png != QgsLayoutExporter.ExportResult.Success:
                print(f"ERRO: falha ao exportar PNG do layout '{layout.name()}' (codigo {resultado_png})", file=sys.stderr)
                sys.exit(1)

            # Compara pelo nome-base (não pelo caminho completo) para não depender de qual
            # separador de diretório (/ ou \) o glob devolve no Windows.
            padrao_paginas = re.compile(rf"^{re.escape(nome_arquivo)}(_\d+)?\.png$")
            paginas_png = sorted(
                p for p in glob.glob(f"{dir_saida}/{nome_arquivo}*.png")
                if padrao_paginas.match(os.path.basename(p))
            )
            for pagina in paginas_png:
                print(f"ARQUIVO_EXPORTADO: {pagina}")

            # PDF: multiplas paginas ficam num unico arquivo.
            caminho_pdf = f"{dir_saida}/{nome_arquivo}.pdf"
            resultado_pdf = exporter.exportToPdf(caminho_pdf, QgsLayoutExporter.PdfExportSettings())
            if resultado_pdf != QgsLayoutExporter.ExportResult.Success:
                print(f"ERRO: falha ao exportar PDF do layout '{layout.name()}' (codigo {resultado_pdf})", file=sys.stderr)
                sys.exit(1)
            print(f"ARQUIVO_EXPORTADO: {caminho_pdf}")
    finally:
        qgs.exitQgis()


if __name__ == "__main__":
    main()
