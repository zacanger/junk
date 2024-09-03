##Step 3: Add Data from your Service to your Controller and Display it
Now that your service is set up, let's inject your service in to your controller then add that data to the scope of your controller, then display it in your view
* Inject your dataService into your mainCtrl
* Use the proper method on your dataService object to get the quotes array then add it to your $scope object in your mainCtrl
* Once the quotes data is on your scope, use ng-repeat to loop over that data in  your index.html page and display it.

##Step 4: Add Options to Filter, Add, and Remove Items from your Quotes Array
* Create three buttons, Add Quote, Remove Quote, and Filter Quotes
* Using ng-click and the methods we set up on our dataService object earlier, make those three buttons do the appropriate action.
* Once you've finished, add some ng-shows to 'toggle' the input boxes for add, remove, and filter making sure you only show one at a time.

##Step 5: Persist your Quotes using your browser's local storage
* Use JSON.stringify and JSON.parse to store the quotes object in your browser's local storage.


