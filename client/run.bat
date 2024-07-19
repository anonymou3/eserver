@echo off

if exist C:\1miniconda3\Scripts\activate.bat (
    call C:\miniconda3\Scripts\activate.bat C:\miniconda3
    conda activate base && python client.py
) else (
    python client.py
)