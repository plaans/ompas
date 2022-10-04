---
description: Functions and algorithms used in RAE.
---

# Algorithms

## **The Philisophy of this implementation of RAE**

The main idea behind this implementation of RAE is to have operational model in lisp. From this representation we will be able to extract a descriptive model to run planning from operational models.

A choice has been made to develop most of the algorithm in lisp. The main loop of rae consist only on creating a new task in the program scheduler, and run an asynchronous evaluation of task code. Defining a task generate lisp code that will contain the algorithms _progress_, _select_, and others

## Main algorithm

RAE main algorithm receives jobs via a stream. A job is an sexpr that will be evaluated in RAE environment.

```
main(context: RAEEnv)
  loop
      new_job <- job_receiver$
      /*creates a new asynchronous task in the program,
      handled automatically by the scheduler*/
      async eval(job)
  end loop
```

## Progress

```
progress(job) -> boolean
    (method, job_id) <- select(job)
    if method == null then
        return false
    else
        if eval(method) == true then
            set-success-for-task(job_id)
            return true
        else
            return retry(job_id)
        end if
    end if
```

## Retry

```
retry(job_id) -> boolean
    next_method <- get-next-method(job_id)
    if next_method == null then
        return false
    else
        if eval(next_method) == true then
            set-success-for-task(job_id)
            return true
        else
            return retry(job_id)
```

## Select

```
select(job) -> (LValue, JobId)
    applicable_instances <- generate-instances(job)
    job_id <- add_job_to_agenda(job, applicable_instances)
    sorted_applicable_instances <- sort_by_score(applicable_instances)
    current_method <- sorted_applicable_instances[0]
    return (current_method, job_id)
```
