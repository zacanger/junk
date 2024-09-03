package main

import (
	"github.com/kataras/iris"
	"github.com/kataras/iris/middleware/logger"
	"github.com/kataras/iris/middleware/recover"
	mongo "gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

type Todo struct {
	Title     string `bson:"title"`
	Completed bool   `bson:"completed"`
}

func main() {
	app := iris.New()
	app.Logger().SetLevel("warning")
	app.Use(recover.New())
	app.Use(logger.New())

	const (
		Host       = ""
		Username   = ""
		Password   = ""
		Database   = ""
		Collection = ""
	)

	session, err := mongo.DialWithInfo(&mongo.DialInfo{
		Addrs:    []string{Host},
		Username: Username,
		Password: Password,
		Database: Database,
	})

	if err != nil {
		panic(err)
	}

	defer session.Close()
	db := session.DB(Database)
	collection := db.C(Collection)

	app.Post("/add", func(ctx iris.Context) {
		var todo Todo
		err := ctx.ReadJSON(&todo)
		if err != nil || todo.Title == "" {
			ctx.StatusCode(iris.StatusBadRequest)
			ctx.JSON(iris.Map{"mesage": "Malformed todo!"})
			return
		}

		collection.Insert(todo)
	})

	app.Get("/todo/{title:string}", func(ctx iris.Context) {
		title := ctx.Params().Get("title")
		var results []Todo
		err := collection.Find(bson.M{"title": title}).All(&results)
		if err != nil {
			ctx.StatusCode(iris.StatusBadRequest)
			ctx.JSON(iris.Map{"message": "oh no!", "error": err})
			return
		}
		ctx.JSON(iris.Map{"results": results})
	})

	app.Get("/completed/{completed:boolean}", func(ctx iris.Context) {
		completed, _ := ctx.Params().GetBool("completed")
		var results []Todo
		err := collection.Find(bson.M{"completed": completed}).All(&results)
		if err != nil {
			ctx.StatusCode(iris.StatusBadRequest)
			ctx.JSON(iris.Map{"message": "oh no!", "error": err})
			return
		}
		ctx.JSON(iris.Map{"results": results})
	})

	app.Put("/completed/{completed:boolean}", func(ctx iris.Context) {
		completed, _ := ctx.Params().GetBool("Completed")
		query := bson.M{"completed": completed}
		var change Todo
		ctx.ReadJSON(&change)
		collection.UpdateAll(query, bson.M{"$set": bson.M{"completed": change.Completed}})
	})

	app.Delete("/title/{title:string}", func(ctx iris.Context) {
		title := ctx.Params().Get("title")
		collection.Remove(bson.M{"title": title})
	})

	app.Run(iris.Addr(":9999"), iris.WithoutServerError(iris.ErrServerClosed))
}
