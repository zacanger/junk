package main

import (
	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
	"github.com/gofiber/helmet"
)

func main() {
	app := fiber.New()

	app.Use(middleware.Logger())
	app.Use(helmet.New())

	app.Get("/hi", func(c *fiber.Ctx) {
		c.Send("bye")
	})

	app.Get("/data", func(c *fiber.Ctx) {
		if err := c.JSON(fiber.Map{
			"foo": "asdf",
			"bar": "ghjkl",
		}); err != nil {
			c.Status(500).Send(err)
			return
		}
	})

	app.Static("/", "./public")

	app.Listen(9999)
}
