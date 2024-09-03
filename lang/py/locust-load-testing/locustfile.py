from locust import HttpLocust, TaskSet, task


class MainTask(TaskSet):
    @task(4)
    def home(self):
        response = self.client.get('/')
        print("Status code:", response.status_code)

    @task(3)
    def foos(self):
        self.client.get('/foo/bar')
        self.client.get('/foo/baz')
        self.client.get('/foo/quux')

    @task(2)
    def bars(self):
        self.client.get('/bar/foo')
        self.client.get('/bar/baz')
        self.client.get('/bar/quux')

    @task
    def quuxes(self):
        self.client.get('/quux/foo')
        self.client.get('/quux/bar')
        self.client.get('/quux/baz')

class Main(HttpLocust):
    task_set = MainTask
    min_wait = 1000
    max_wait = 8000
