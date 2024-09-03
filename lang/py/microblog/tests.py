from datetime import datetime, timedelta
import unittest
from app import app, db
from app.models import User, Post


class UserModelSuite(unittest.TestCase):
    def setUp(self):
        app.config["SQLALCHEMY_DATABASE_URI"] = "sqlite://"
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()

    def test_password_hashing(self):
        u = User(username="zac")
        u.set_password("asdf")
        self.assertFalse(u.check_password("ghjkl"))
        self.assertTrue(u.check_password("asdf"))

    def test_follow(self):
        u1 = User(username="one", email="one@one.com")
        u2 = User(username="two", email="two@two.com")
        db.session.add(u1)
        db.session.add(u2)
        db.session.commit()
        self.assertEqual(u1.followed.all(), [])
        self.assertEqual(u2.followed.all(), [])

        u1.follow(u2)
        db.session.commit()
        self.assertTrue(u1.is_following(u2))
        self.assertEqual(u1.followed.count(), 1)
        self.assertEqual(u1.followed.first().username, "two")
        self.assertEqual(u2.followers.count(), 1)
        self.assertEqual(u2.followers.first().username, "one")

        u1.unfollow(u2)
        db.session.commit()
        self.assertFalse(u1.is_following(u2))
        self.assertEqual(u1.followed.count(), 0)
        self.assertEqual(u2.followers.count(), 0)

    def test_follow_posts(self):
        u1 = User(username="one", email="one@one.com")
        u2 = User(username="two", email="two@two.com")
        db.session.add(u1)
        db.session.add(u2)
        now = datetime.utcnow()

        p1 = Post(
            author=u1,
            body="testing",
            timestamp=now + timedelta(seconds=1))
        p2 = Post(
            author=u2,
            body="testing still",
            timestamp=now + timedelta(seconds=2))
        db.session.add(p1)
        db.session.add(p2)

        u1.follow(u2)

        db.session.commit()

        self.assertEqual(u1.followed_posts.all(), [p2, p1])
        self.assertEqual(u2.followed_posts.all(), [p2])
