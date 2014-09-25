# Extract data from cherry growers receipts
#
# Usage:
# bash export_growers_data.sh
#
# Run in a directory with Growers Receipt pdfs.
# It will produce one txt file for each pdf with the OCR'd text from the first page.

for pdf in *.pdf
  do
    filename=$(basename "$pdf")
    filename="${filename%.*}"

    # Convert pdf to image
    convert -density 300 $pdf $filename.png

    # OCR the png and extract the text
    tesseract $filename-0.png $filename
  done
