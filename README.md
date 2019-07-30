# estrattore
script e programmi per analisi e visualizzazione dei dati estratti con estrattore REM

# INDICE

* plot_termo_tutti.R 

plotta tutti i file di dati (rilevati) elencati nel file di richiesta dell'estrattore, contenuti nella directory dove risiede lo script generando un grafico unico per ciascun sensore ma differenti funzioni; contemporaneamente cerca minimo e massimo assoluto della serie e loro marca temporale e li scrive in un file di testo per più facile elaborazione successiva. 

Di seguito la query per individuare i termometri assegnati a XX attivi dal ... al ...

SELECT distinct(staz.IDstazione), IDsensore, Provincia, CONCAT(Comune," ",IFNULL(Attributo,"")), IDsensore , Acronimo 
FROM  A_Stazioni as staz, A_Sensori as sens , Utenti as ut, StazioniAssegnate as ass 
WHERE ass.IDstazione=staz.IDstazione and ass.IDutente=ut.IDutente and staz.IDstazione=sens.IDstazione 
AND NOMEtipologia="T" 
AND sens.DataInizio<"YYYY-MM-GG" 
AND (sens.DataFine is null or sens.DataFine>"YYYY-MM-GG") 
AND IDrete in (1,4) 
AND Acronimo="XX" 
ORDER BY Provincia;

Copiare nella directory dove risiedono i dati dell'estrattore, oltre al file di richiesta, anche il file "AnagraficaSensori.csv" generato ogni giorno nelle cartella di rete \\ ... \ARPA\ ... \Sede Centrale\PROGETTI\REM\Anagrafica
