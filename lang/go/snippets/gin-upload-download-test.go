package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"os"
	"time"

	"github.com/gin-gonic/gin"
)

type File struct {
	Name string `uri:"name" binding:"required"`
}

func main() {
	router := gin.Default()
	api := router.Group("/api")
	v1 := api.Group("/v1")
	files := v1.Group("/files")

	files.POST("/", func(c *gin.Context) {
		file, err := c.FormFile("file")
		if err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err.Error()})
		}

		n, err := Upload(file)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}
		c.JSON(http.StatusOK, gin.H{
			"success": "uploaded",
			"name":    fmt.Sprintf("%s", n),
		})
	})

	files.GET("/:name/", func(c *gin.Context) {
		var file File
		if err := c.ShouldBindUri(&file); err != nil {
			c.JSON(http.StatusBadRequest, gin.H{"error": err})
			return
		}
		m, cn, err := Download(file.Name)
		if err != nil {
			c.JSON(http.StatusNotFound, gin.H{"error": err})
			return
		}
		c.Header("Content-Disposition", "attachment; filename="+m)
		c.Data(http.StatusOK, m, cn)
	})

	router.Run(":9999")
}

// TODO: fill this in
var base = "/home/whatever/somewhere"

func Upload(file *multipart.FileHeader) (string, error) {
	src, err := file.Open()
	if err != nil {
		return "", err
	}
	defer src.Close()

	n := fmt.Sprintf("%d - %s", time.Now().UTC().Unix(), file.Filename)
	dst := fmt.Sprintf("%s/%s", base, n)
	out, err := os.Create(dst)
	if err != nil {
		return "", err
	}
	defer out.Close()

	_, err = io.Copy(out, src)
	return n, err
}

func Download(n string) (string, []byte, error) {
	dst := fmt.Sprintf("%s/%s", base, n)
	b, err := ioutil.ReadFile(dst)
	if err != nil {
		return "", nil, err
	}
	m := http.DetectContentType(b[:512])
	return m, b, nil
}
