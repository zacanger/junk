# mini-birds-related
## Objectives
This project will build out the capabilities of our mini-project from yesterday to help solidify our understanding of using data relationships in MongoDB through Mongoose. By the end of the project, you should know how to utilize Mongoose models to set up both embedded and referential relationships.

We are going to be working with three different Collections -- Sighting, Bird, and User. When we are finished, users of our API should be able to:
  * POST new sightings, birds, or users
  * Edit (PUT) existing sightings, birds, or users by id
  * DELETE existing sightings, birds, or users by id
  * GET a list of birds or users in our database
  * GET a list of sightings that has populated user data. Sightings should be able to be queried by:
    - **a specific user**
    - the bird name
    - if the sighting has been confirmed or not

We will need to:
  * Add a User model
  * Add a Bird object to be exported to our Sighting Model
  * Refactor our Sighting model
  * Create endpoints for User CRUD operations
  * Adjust our endpoints that work with the Sighting model to the new data structure and populate requirements

## Step 1: Understanding our Data Structure
In storing data in three different collections we will need to have relationships to best utilize our data.

Review the two `.json` files included in this repo to get familiar with the data structure we will be creating. Identify the type of relationships we will be creating.

## Step 2: Create the _Bird_ object to be embedded into the Sighting model
In a new file, `Bird.js`, create a Bird object using some the schema properties from the existing Sighting model. Name, order, and status will be the properties moved to our Bird object. See the bird property in the `sighting.json` file for guidance on the properties to be included in the Bird object.

## Step 3: Create the _User_ Model
In a new file, `User.js`, create a User model with the schema properties email, username, level, location, and member. Refer to the `user.json` file for needed types and validations.

**Consider** adding a hook for an `updatedAt:` field

## Step 4: Refactor our _Sighting_ Model
Add a property to the Sighting schema called `user:` that will create a relationship between a User and and Sighting. Each Sighting should be required to be related to only one User. A user may have multiple sightings.  

>  Using a **referential** relationship for users will help us keep up-to-date information on a user for each sighting. For example, if a user was to update their email address all of their sightings will now refer to the updated user object with the newly updated email address! No need to change an email on every sighting! No stale data!

Add another property called `bird:` that will store embedded data related to a specific bird when a new sighting is created.

> Using a **embedded** relationship to store bird data for a given sighting will allow us to keep historical data. For example, if the Osprey became a threatened species and we updated it's status in our database to reflect that, we would be able to compare the number of Osprey sightings from when they had the status of 'least concern' to when they had the status of 'threatened' -- crucial information for any ornithophiliac.


## Step 5: Endpoints and Controllers - Create and Refactor

  * Create CRUD endpoints and a Controller with CRUD methods for User
  * Refactor the endpoints and logic related to Sightings from yesterday's code. Make any needed adjustments to existing code.
  * When Sighting data is requested make sure to populate it with User data before returning it to the client.
  * Make it possible for the client to request sightings for a specific user by through sending the user id as a part of the request query.

Test your endpoints with POSTMan and RoboMongo to ensure that everything is happening as expected!

---

BOOM! You're a relationship guru ...
