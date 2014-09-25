# Extract data from  cherry growers receipts

# Convert pdf to image
convert -density 300 CherryReceipt.pdf CherryReceipt.png

# OCR the png and extract the text
tesseract CherryReceipt-0.png CherryReceipt

