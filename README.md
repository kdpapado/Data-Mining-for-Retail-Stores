# Data-Mining-for-Retail-Stores
A project in RStudio
ΕΞΟΡΥΞΗ ΓΝΩΣΗΣ ΑΠΟ ΔΕΔΟΜΕΝΑ ΣΥΝΑΛΛΑΓΩΝ ΚΑΤΑΣΤΗΜΑΤΟΣ ΛΙΑΝΙΚΗΣ

Άσκηση 1. Μετασχηματισμός πρωτογενών δεδομένων
Αρχικά φορτώνουμε τα δεδομένα στο RStudio με την εντολή read.csv(). Στη συνέχεια επιλέγουμε τα 13 προϊόντα που ζητήθηκαν. Έπειτα, μετασχηματίζουμε τα παραπάνω πρωτογενή δεδομένα σε μορφή κατάλληλη για την εφαρμογή των μεθόδων κανόνων συσχέτισης της R, δηλαδή σε δυαδική μορφή συναλλαγών, όπου κάθε μοναδική τιμή της κάθε υπάρχουσας μεταβλητής θα μετατραπεί σε νέα, ξεχωριστή δυαδική μεταβλητή. Στην συνέχεια εισάγουμε τα δεδομένα αυτά στην R.

Για να μπορέσουμε να χρησιμοποιήσουμε και την αξία της συναλλαγής( basket_value) στους κανόνες συσχέτισης αργότερα , την διακριτοποιούμε σε τρείς (περίπου) ισοπληθείς κατηγορίες, τις low_value_basket, medium_value_basket και high_value_basket. 
Για να επιτύχουμε αυτήν την διακριτοποίηση χρησιμοποιούμε την quantile() και την cut() και στην συνέχεια εμφανίζουμε το πλήθος της κάθε κατηγορίας με την table().
 
 
Για να παράγουμε την δυαδική μορφή των νέων διακριτοποιημένων μεταβλητών (ποιοτικών πλέον) επαναλαμβάνουμε τη διαδικασία που κάναμε προηγουμένως αλλά ορίζουμε μια συνάρτηση για αυτή, την οποία καλούμε έπειτα στη νέα διακριτή μεταβλητή basket_value_bin και μετά την αφαιρούμε. 

Άσκηση 2. Μάθηση κανόνων συσχέτισης με την μέθοδο Apriori
α) Αφού κάναμε την απαραίτητη προεπεξεργασία –καθαρισμό, μπορούμε να προχωρήσουμε στην παραγωγή κανόνων συσχέτισης. Αρχικά, εγκαθιστούμε το πακέτο arules και αργότερα παράγουμε τους κανόνες με την μέθοδο apriori. Στην αρχή, θέτουμε το κατώτατο support = 0.001.
 
Για την εμφάνιση μόνο των πρώτων είκοσι κανόνων χρησιμοποιήσαμε την εντολή inspect(rules[1:20]):
 

Έπειτα αυξάνουμε το κατώτατο support στο 0.002 και μετά μειώνουμε το κατώτατο support στο 0.0002 και βλέπουμε τους αντίστοιχους κανόνες σε κάθε περίπτωση.
 

β) Στη συνέχεια, βρίσκουμε τους είκοσι κανόνες με το υψηλότερο confidence αποκλειστικά για τα προϊόντα (στήλες 4-16 του groceries_discrete), κάνοντας μια ταξινόμηση των κανόνων ως προς το confidence και επιλέγοντας τους 20 πρώτους.
 
Παρατηρούμε ότι ο 13ος συνδυασμός, δηλαδή ο {citrus fruit, rolls/buns, bottled water, pastry} ⇒{chocolate} ενώ έχει τον μεγαλύτερο βαθμό εξάρτησης (lift )μεταξύ των δύο γεγονότων LHS και RHS, παρ’ όλα αυτά βλέπουμε ότι το support του είναι μικρότερο σε σχέση με άλλους κανόνες που έχουν μικρότερο lift. Αντίθετα, ο συνδυασμός {chocolate, bottled water, sausage} ⇒ {other vegetables}, για παράδειγμα, ενώ έχει μικρότερο lift συγκριτικά με τον προηγούμενο συνδυασμό, έχει μεγαλύτερο support.
γ) Βρίσκουμε τώρα τους είκοσι κανόνες με το υψηλότερο confidence για τα προϊόντα και την διακριτοποιημένη αξία καλαθιού (στήλες 4-19 του groceries_discrete), κάνοντας μια ταξινόμηση των κανόνων ως προς το confidence και επιλέγοντας τους 20 πρώτους.
 
Από τα παραπάνω αποτελέσματα αντιλαμβανόμαστε ότι πιθανόν το ακριβότερο προϊόν να  είναι το sausage, διότι στους apriori κανόνες που έχουν στο RHS την {high_value_basket}, το sausage έχει τις περισσότερες εμφανίσεις. 




Άσκηση 3. Ομαδοποίηση συναλλαγών με χρήση μεθόδου k-means
α) Στο επεξεργασμένο σύνολο δεδομένων εφαρμόζουμε την μέθοδο ομαδοποίησης k-means στα δύο συνεχή χαρακτηριστικά basket_value και recency_days (στήλες 2 και 3 από τον groceries_discrete) και εξάγουμε πέντε (5) ομάδες συναλλαγών.
 
To αντικείμενο που μας επέστρεψε ο k-means μετά το clustering είναι μια λίστα που περιέχει μεταξύ άλλων και το διάνυσμα με τις αναθέσεις των 7537 πελατών στις ομάδες ($cluster) και τα κέντρα (μέσες τιμές) της κάθε ομάδας ($centers). Το κέντρο της κάθε ομάδας μας επιτρέπει να καταλάβουμε το προφίλ της και τις διαφορές της από τις υπόλοιπες.
 
β) Η μέση τιμή των κέντρων των ομάδων που βγήκαν:
 
Για την Ομάδα 1 είναι (7.875556, 22.139918), για την Ομάδα 2 είναι (2.912685, 54.591659), για την Ομάδα 3 είναι (3.880279, 37.978636), για την Ομάδα 4 είναι (5.421630, 70.541797) και για την Ομάδα 5 είναι (4.872536, 6.864149).
 
Παρατηρούμε ότι η Ομάδα 1 είναι ομάδα σχετικά παλιών συναλλαγών μεγάλης αξίας, η Ομάδες 2 και 3 είναι ομάδες παλιών συναλλαγών μικρότερης αξίας σε σχέση με τις άλλες ομάδες και η Ομάδα 5 είναι ομάδα πιο πρόσφατων συναλλαγών μέτριας αξίας. Αυτό που κάνει εντύπωση είναι η Ομάδα 4, η οποία είναι η ομάδα των πιο παλιών συναλλαγών και αποτελεί την ομάδα με την δεύτερη μεγαλύτερη αξία συναλλαγών. Αυτό σημαίνει ότι το Τμήμα Μάρκετινγκ θα έπρεπε να ασχοληθεί με αυτήν την ανησυχητική ομάδα, έτσι ώστε να δει τι είναι αυτό που έκανε τους πελάτες, που ανήκουν σε αυτήν την ομάδα, να σταματήσουν να αγοράζουν και κατ’ επέκταση να το αντιμετωπίσει.
γ) Στη συνέχεια, εξάγουμε τις αναθέσεις της κάθε μιας συναλλαγής σε μια νέα ποιοτική μεταβλητή (στήλη), έτσι ώστε να είναι εφικτή η μάθηση κανόνων συσχέτισης και σε αυτό το νέο χαρακτηριστικό. Η ονομασία αυτού του νέου χαρακτηριστικού είναι “Cluster”. Έπειτα, μετατρέπουμε και αυτήν την μεταβλητή για να έχει την κατάλληλη μορφή για εφαρμογή κανόνων συσχέτισης. Δηλαδή, αποθηκεύουμε την ομάδα της κάθε συναλλαγής, με την χρήση πέντε (5) χαρακτηριστικών-μεταβλητών (Cluster1,Cluster2, Cluster3, Cluster4 και Cluster5) για να είναι εφικτή η εφαρμογή των κανόνων συσχέτισης. Παράγουμε δηλαδή και πάλι την δυαδική μορφή των συναλλαγών.

Άσκηση 4. Συνδυαστική αξιοποίηση μεθόδων: περιγραφή προϊοντικού προφίλ ομάδων με χρήση κανόνων συσχέτισης
Αρχικά βρίσκουμε τους είκοσι (20) κανόνες με το υψηλότερο confidence αποκλειστικά για τα προϊόντα και τις ομάδες.
 

Η αρίθμηση των ομάδων ξεκινάει από το 0 άρα η Ομάδα 1 είναι η Cluster0 και η Ομάδα 5 είναι το Cluster4. Παρατηρούμε ότι η Ομάδα 1 αγοράζει κυρίως pastry, καθώς και ό, τι αγοράζει και η Ομάδα 5. Η Ομάδα 2 αγοράζει ό, τι και η Ομάδα 5. Η Ομάδα 3 αγοράζει sausage και ό, τι αγοράζουν οι ομάδες 4 και 5 (δηλαδή τα Cluster3 και Cluster4 αντίστοιχα). Η Ομάδα 4 αγοράζει pastry και ό, τι αγοράζει η ομάδα 5. Η Ομάδα 5 αγοράζει κυρίως chocolate, pastry, rolls/buns, cream, tropical fruit, sausage, pastry, καθώς και ό, τι αγοράζουν οι υπόλοιπες ομάδες.
Η ανησυχητική ομάδα συναλλαγών που βρήκαμε στην Άσκηση 3, δηλαδή η Ομάδα 4 (Cluster3), σχετίζεται συνήθως με το προϊόν pastry. Απ’ ό, τι φαίνεται είχε μεν μεγάλο confidence (=1) αλλά δεν είχε αρκετά μεγάλο support(είναι περίπου ίσο με 0.1161).
