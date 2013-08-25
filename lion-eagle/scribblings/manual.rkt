#lang scribble/manual



@title{Lion-Eagle}
@author+email["Ryan Plessner" "rpless@ccs.neu.edu"]

@section{Introduction}

Lion Eagle is a MVC framework for racket/gui/base.

The Github source repository can be found at @url{https://github.com/rpless/lion-eagle}.

@section{Getting Started}

In order to wire up a MVC in Lion Eagle, you need to create the Model, the View, and the Controller.
Lion Eagle provides a mechanism to create each of these that is far less verbose than creating these manually in Racket.
In order to demonstrate some Lion Eagle's features we will step through creating a simple application.

@subsection{The Model}
@defmodule["lion-eagle/model.rkt"]

The model module provides the means to create simple models.

@defform[(define-model id [field ...])]{
The id is an identifier that is the name of the model.
Each field is an identifier for a field in the model.
This form creates a signature for the model, which includes getters and setters for each field.
In addition, it creates a function that creates a default implementation of the model.
}

@filebox["counter-model.rkt"]{
@codeblock|{
#lang racket

(require "../lion-eagle/model.rkt")
(provide count-model make-count-model)

(define-model count-model (count))
}|}

Here, we define a model called count-model. It has a field called count. For our purposes, the count field will be an integer.
            
Although contracts are not currently supported, something is in the works for this.

@subsection{The Controller}
@defmodule["lion-eagle/controller.rkt"]

The controller module provides a form for creating a model's controller.

@defform[(define-controller id action ...)
         #:grammar ([action (define/action (id arg ...) body ...)])]

@subsection{The View}
@defmodule["lion-eagle/view.rkt"]

The view module provides a form for creating heirarchical Graphical User Interfaces.

@defform[(view controller-id (frame id title subcomponents ...))
         #:grammar ([subcomponents (message id (bind field model->string))
                                   (button id (action action-id))
                                   (textfield id (bind field model->string string->model))])]

The view form creates a unit that imports the controller specified by controller-id.
The unit exports a @racket[view-factory^]

@defsignature[view-factory^ ()]

            
            
            