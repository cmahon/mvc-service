To Do List
==========

General

* More examples
* Tests
* Benchmarks
* Haddock documentation

Service

* Review alternative design options for ManagedService e.g. ManagedService (Managed (View b,Controller a))
* Keeps and handles
* Experiment with seal, performGC and wait for termination
* Service/app execution
* Local/async service deployment options (encapsulate model and service?)
* Lenses for Service
* Service resume command needed (vs reuse service start in paused status)

Model

* Retire Model (EventHandler replaces it when dependencoes migrated)
* listt version, simplified version
* Add AppStateAPI to processEvent
* Generalised recursive processing interface (e.g. like ArrowLoop)
* Sample event handlers e.g. logger

Event

* Benchmark existential event unwrapping/casting overhead

Socket

* Review all handlers to determine whether connection (socket wrapper) and connection status (bool queue) should be updated, if only as a precaution against client not performing this cleanup as expected. May require a review of whether updating a boolean queue is the ideal design as it may result in multiple identical values.
* Complete model (e.g. checking and updating state)
* Tidy up Connection code, consistent use of STM vs IO
* Streamline verbose code e.g. atomically
* Secure SSL option
* Command to change socket host/port
* Tests and benchmarks























