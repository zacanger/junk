#!/usr/bin/env python2

from datetime import datetime
from collections import namedtuple

NAME = "Zac Anger"
EMAIL = "zac at zac anger dot com"

class JobMixin(object):
    def __repr__(self):
        return "{0} - {1}, {2}, {3}, {4}\n{5}\n{6}".format(
            self.started.strftime("%B %Y"),
            self.left if isinstance(self.left, str) else self.left.strftime("%B %Y"),
            self.company,
            self.location,
            self.title,
            "\n".join(self.description),
            "=" * 20
        )


# Experience

class Jane(JobMixin):
    company = "Jane.com, LLC"
    location = "Lehi, UT"
    title = "Software Engineer"
    started = datetime(2016, 5, 16)
    description = [
        "Worked on product teams building front-end apps including the React rewrite of Jane.com and internal apps."
        "Worked on an infrastructure team on deployments, CI, and testing."
        "Used React, CSS, CSS-in-JS libraries, Linux, Koa, Express, Redux, AWS, Docker, Graphql and Apollo, Flow, Typescript, Nginx, and Angular."
    ]

class DevMountain(JobMixin):
    company = "DevMountain"
    location = "Provo, UT"
    title = "Mentor & Junior Developer"
    started = datetime(2016, 2, 1)
    left = datetime(2016, 6, 1)
    description = [
        "Mentored group of new developers; planned and carried out lectures; curriculum maintenance and prep; discussion and toy-problem leading."
        "Internal DevMountain projects, including technologies such as Node, Express, Angular, Chromium (extensions), CSS/Sass/Less, and more."
    ]


class Mickeys(JobMixin):
    company = "Mickey's Pizza"
    location = "York, PA"
    title = "Web Development & Design"
    started = datetime(2014, 9, 1)
    left = datetime(2015, 9, 20)
    description = [
        "Full revamp of wholesale foods company web presence.",
        "Design, front-end development, and deployment."
    ]


class Music(JobMixin):
    company = "Menchey Music & RLH Guitars"
    location = "PA"
    title = "Sales,  Venue Management"
    started = datetime(2005, 12, 1)
    left = datetime(2008, 7, 1)
    description = [
        "Lead combo sales",
        "Mix Engineer",
        "Recording engineer",
        "Live sound engineer",
        "Guitar teacher",
        "Drum teacher",
        "Venue management",
        "Guitar repairs",
        "Custom drumset builds"
    ]


class SchoolMixin(object):
    def __repr__(self):
        return "{0} - {1}, {2}, {3}, {4}\n{5}\n{6}".format(
            self.started.strftime("%B %Y"),
            self.left if isinstance(self.left, str) else self.left.strftime("%B %Y"),
            self.name,
            self.location,
            self.course,
            "\n".join(self.description),
            "=" * 20
        )

# Education
#
class Devmtn(SchoolMixin):
    name = "DevMountain"
    location = "Provo, UT"
    course = "Full-time Immersive Javascript Development Course"
    started = datetime(2015, 10, 13)
    left = datetime(2016, 1, 22)
    description = [
        "An intensive bootcamp-style course on Javascript.",
        "Strong focus on the MEAN stack.",
        "Built several front- and back-end projects, accumulating over a thousand NPM installs in the first month",
        "Worked solo and in groups, developing and designing production-ready full-stack applications and services"
    ]

Project = namedtuple("Project", "name description")

if __name__ == "__main__":
    experience = [DevMountain(), Mickeys(), Music()]
    print NAME
    print EMAIL
    print "Experience:"
    for exp in experience:
        print exp

    print "Education:"
    eduction = [Devmtn()]
    for edu in eduction:
        print edu

    print "Projects & Interests:"
    projects = [
        Project("Github account", "github.com/zacanger"),
        Project("Linkedin profle", "https://www.linkedin.com/in/zacanger")
    ]
    for pro in projects:
        print pro

    print "Languages: Javascript (React, Node, ES6, ES7, Angular, jQuery, and even vanilla!), Bash, Python (just a wee bit), HTML, CSS."
    print "Technologies: Git, various Node frameworks, Vue, Functional Reactive Programming, CSS Preprocessors."
    print "Summary: Web enthusiast, professional musician, OSS supporter, nix nerd, and Node fanboi from the east coast."
