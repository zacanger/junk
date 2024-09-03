# mini-birds
## Objectives
The purpose of this project is to solidify your understanding of the MongoDB API.  By the end of the project, you should know how to perform CRUD operations in MongoDB and view your data (via command line or a GUI).

We are going to build a bird-sighting API.  Consumers (or, developers who use this API) will be able to:

 * Send requests to the API to report a bird sighting
 * Send requests to the API to retrieve bird sightings in a specific area, or of a specific species
 * Send requests to change/edit/update a previously reported sighting
 * Send requests to delete/remove a previously reported sighting

We will need to set up our app, create a Node/Express API, and then hook the API up to our database.

## Step 1: Setting Up

Initialize your Node app and install the following packages:
 * `express`
 * `body-parser`
 * `cors`
 * `mongojs`

Initialize your app and set up your middleware.

After you have initialized your app, connect to your database via MongoJS.  This is where you will specify the name of your database and the names of your collections (for now, we'll just use a `products` collection).  If you need some guidance, take a look at their [documentation](https://github.com/mafintosh/mongojs).

**Breakpoint:** At this point, you should be able to start your app (`node server.js`) and see that your app is listening, and have no errors.  Your app should also be connected to your database, but we will test that functionality later on.

## Step 2: Create API

Create 'POST', 'GET', 'PUT', and 'DELETE' endpoints for `/api/sighting`

In POSTMan, test each of these endpoints with:

 * **POST** `/api/sighting`
 * **GET** `/api/sighting?region=america&species=redrobin`
 * **PUT** `/api/sighting?id=09evasd09jhahs9d8h9vh`
 * **DELETE** `/api/sighting?id=09evasd09jhahs9d8h9vh`

**Breakpoint:** You should be able to hit these endpoints without error.  To make sure they're actually running correctly, put `console.log` in your functions and hit those endpoints with POSTMan to see that they're running.  For the routes that take queries, `console.log` those queries and make sure you're getting them correctly.  We have not connected these routes to the database yet.  We will add that functionality in the next step.

## Step 3: Connect API to MongoDB

NOTE: I will refer to two different types of queries here.  If I say request query, I mean the query supplied from the front-end via the URL.  Otherwise, I mean a MongoDB query.

 - In your POST route handler, create a new document under the `sightings` collection.  If you are confused how the logic for this might look, refer back to the [docs](https://github.com/mafintosh/mongojs#api). Send an appropriate response to the client (success, or error).

**Breakpoint:** You should be able to save data to your database now.  After posting one or two pieces of dummy data, check to see that that data now exists, using either the command line or a GUI like RoboMongo. If you can see data in your database, you will know that you are correctly connected to the database, and that it is saving correctly.

 - In your GET route handler, create a MongoJS query that will return sightings.  If there is a region specified in the request query, return only the sightings in that region.  If the request query specifies a species, return only the sightings of that species.  If there are no request queries, return all of the sightings.

 - In the PUT route handler, update *x* document (where *x* is the id supplied by the request query) with the data provided in `req.body`.

 - In the DELETE route handler, delete *x* document where *x* is the id supplied by the reqeust query.

**Breakpoint:** Test each of your endpoints with POSTMan and either the command line or RoboMong to ensure that everything is happening as expected.  If you need some dummy data, it is provided in the `birds.js` file.

===========

Congrats!  You've just created your first true CRUD app.  You can now create, read, update, and delete bird sightings, and they will persist, even if you refresh your browser or reboot your server.  Woo!
