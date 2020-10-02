# <div align="center">🦉 Hoot</div>

![Haskell CI](https://github.com/i-am-tom/hoot/workflows/Haskell%20CI/badge.svg?branch=main)

Lots of static site generators exist, but most of them squarely target _expert
users_. They rely on you understanding __HTML__ or __Markdown__, and usually
require you to be comfortable committing to a Git repository.

When we work with __non-technical__ users, however, this becomes impractical;
perhaps our users don't understand these technologies, or we don't want to give
them access to our repository? At this point, we usually end up reaching for a
more complete CMS, such as Wordpress, which has a much more user-friendly
interface, but requires much more engineering effort to host and maintain.

__Hoot__ uses [Contentful](https://www.contentful.com) to try to provide a
minimal static site generator while maintaining all the user convenience of a
more established CMS. By pulling your content model from Contentful, and
rendering it with [Mustache](http://mustache.github.io/), we can give our
content creators a fully-featured editing platform without having to host
anything more than static HTML.

---

# 👑 Features

## All Types Welcome

Hoot interprets your content according to your content model: there are no
restrictions on the content that you can create, or the references between
them! As long as you have a Mustache template to match a content type, Hoot
will render it. You can even render nested content types according to the
parent: render your shop products as full pages or thumbnails without any
duplication in your content library!

## Authors = Designers

Using Contentful's [composable entries](https://www.contentful.com/blog/2018/04/25/get-up-to-speed-on-composable-entries/)
approach, authors can create complex content pages out of small, composable
units. You design the template for each block, and let your authors plug them
together.

## Live Development

Make changes to your templates, and see the effects in real time. Hoot can
watch your templates for changes, and recompile the relevant entries
immediately. This means you can see the effect of every change on all the
content you already have.

## Easy Deployment

Hoot can run as a GitHub action on a cron job, meaning that your live site is
never far behind your author's changes. Need more immediate updates? Hoot can
be called from a [webhook](https://www.contentful.com/developers/docs/concepts/webhooks/)
to rebuild your site the moment a change is published by your authors.

# 🚧 Development

This is very much a WIP, and I'll be committing to it as I get various pieces
ready. Once I have a stable first release, I'll make a more active call for
help, but it's definitely a hobby project for now 🙂
