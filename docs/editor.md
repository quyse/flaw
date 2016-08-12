# FLAW Editor

FLAW Editor is a GUI developer tool for visual editing of game assets.

Editor is based on FLAW Oil technology, which is a way to store and transfer structured data.

**Some stuff is to be implemented yet**. The document is mostly about vision, and not a feature list.

## Idea

In a few words the idea is to have a workflow similar to Google Docs. An implementation, however, has to take into account usually much bigger scale of game data, and have easily extendable data model.

All game data are stored inside special versioned repository of FLAW Oil format on a company's server. With help of an `oild` daemon, the repository can be easily cloned onto client machines. Client repository contains the latest versions of all data (i.e. no history). Users may work with repository by means of the command line utility `oil` or by using `flaw-editor` which is an interactive GUI tool.

Having a cloned locally repository, user can make changes to it. In the same time the editor performs automatic synchronization between server and client, so locally made changes are propagated to server and then to other clients, and changes from other clients are synced into local copy.

The key difference between Oil repository and other collaborative editors and version control systems is that Oil doesn't perform complex merging. The only merging process happens at the low level, as Oil repository is basically a set of key-value pairs (called records). So, record's value is the value brought by the latest change to this record; i.e. latest change always wins. If there were multiple changes to a single record happening simultaneously, record will simply get the latest value as it's arrived on server. The idea is that data is *enough structured* to make such incidents: 1) rare; 2) making small damage. Users have to watch out what they are changing, and if someone just changed the same thing (editor provides help with this). And if merge conflict has actually happened, it must be easy to notice that immediately, and discuss the issue with that other user, and fix/rollback the value to what it needs to be.

Consequently, there's an artificial limitation that client can make changes only if they have working network connection to server, so they will be able to sync their changes immediately, and see changes of others. That's the strategy: see conflict immediately and be able to fix it.

## Repo Format

### Low Level Repo Format

Oil repository is basically a map from bytestring keys to bytestring values. These key-value pairs are called records. There're a few rules:

* Keys are unique, so there's zero or one record with any given key.
* Key's length cannot be zero.
* Record with zero-size value is indistinguishable in the end from non-existent record. To delete a record, simply write zero-length bytestring as a record's value. Trying to read by key a non-existing record returns zero-length value and doesn't trigger any error. Note however that missing record and existing record with zero-size value are still stored differently in the repo, that makes a difference for repo stacks, see below.

Server repo assigns increasing 64-bit integer revision number for every repo change. Client repo knows for what revision it's synced up to, which allows to do sync efficiently.

Sync algorithm works over HTTP. Client simply makes a HTTP request specifying its latest revision and changes to push, and gets new changes in response. In case multiple changes to a single record have happened since client's revision, server sends only the latest change, therefore saving traffic.

Repo doesn't support git-style branching. Instead *repo layering* feature is implemented. Multiple repos may be loaded simultaneously, making up a repo stack. Writes are only made to top repo on the stack. Reads return the first existing value for the given key going from top to bottom of the stack. The feature allows you, for example, to make an additional layer over master repo, make experiment changes and see how it works. Master repo may be still updated by users working on it, and changes will be still immediately visible to you unless overriden by additional layer. Later you can merge the layer with underlying layer, making the layer look like a feature branch in git.

### Middle Level Repo Format

Middle level is a level of entities working on top of low level.

Every entity is stored in one or more low-level records. Every entity has an entity id bytestring, so all records related to an entity has keys prefixed with entity id.

Main entity record is a record with key equal to entity id. Main entity record's value contains entity type id, and optionally additional data (depending on entity type). Entity type id strongly identifies what Haskell data type represents this entity. All entity data types implement `Entity` typeclass. Every entity can incrementally update its value given the change, i.e. key-value pair.

There's a special type of entities - entity tag. Entity tag is an entity too, but it is somewhat special. Every entity tag is attached to some other entity, so tag's entity id consists of constant entity tag id and entity id of entity it's attached to. Therefore there's only zero or one tag of each particular tag type for each entity. Tags allow to attach additional optional information to entity without need to change entity's format.

## Editing

### Project

Local connection settings for editor like remote repo url, path to local repo, username/password/certificate for authentication are stored in a so-called project file. Project also includes list of enabled repo plugins with their configurations.

### Authentication

Currently authentication is not implemented and up to the server. For example, you can leverage `nginx` caching web server to check client's certificate and reject unauthenticated requests to `oild`.

There is no discretionary access control. If user has write access to repo, they can change any record without limitations. It means that every user must be trusted. In case of misbehavior repo history can be used to recover data.

### Sessions

Once editor is connected to remote repo, it creates or uses existing session. Session is a special entity storing workspace settings and some identification of a user. Editor automatically updates session object. Also editor creates `Editing` tags for objects for which user have a subeditor opened or in any other sense "is about to change" the entity. Editing tags are discovered and shown by editors providing users with information about other users' actions.

## Repo Plugins

Users can locally enable additional functionality per repo represented as repo plugins. Repo plugin is a pluggable handler for all records in repo. It got triggered for every change to repo and is free to use its own database of any sort to process or analize repo data.

Possible examples of repo plugins:

* Reference Database collects information on entities referencing each other, and for example is able to find all entities referencing a given entity. Also is able to do garbage collection, i.e. remove all unreferenced entities.
* Full Text Search indexes all entities' text data and allows user to quickly look for an entity by typing string in. May have settings for language or stemming rules.
