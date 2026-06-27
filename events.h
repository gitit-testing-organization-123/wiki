
/*
 * Example of event registration:
 *
 *  event_register(
 *    (Event) {
 *      0,
 *      1,
 *      iter1,
 *      {iter1_expr0},
 *      ((int *)0),
 *      ((double *)0),
 *      "events.c",
 *      12,
 *      "iter1"
 *     }
 *  );
 *
 *  static int iter1_expr0(int *ip,double *tp,Event *_ev){
 *   int i=*ip;
 *   double t=*tp;
 *   int ret=(i++)!=0;
 *   *ip=i;
 *   *tp=t;
 *   return ret;
 * }
 *
 * static int iter1(const int i,const double t,Event *_ev)
 * {
 *   tracing("iter1","events.c",12);
 *   {
 *     // Event body
 *     printf("iter1");
 *   }
 *   {
 *     end_tracing("iter1","events.c",14);
 *     return 0;
 *   }
 *   end_tracing("iter1","events.c",14);
 * }
 *
 */

#ifndef HUGE
#define HUGE 1e30f
#endif

typedef struct
{
  int iter; /**< current iteration */
  double t; /**< current time */
} EventCursor;

typedef struct
{
  int next_iter;    /**< next iteration at which any event is due */
  double next_time; /**< next time at which any event is due */
} EventNext;

typedef struct
{
  int next_iter;    /**< next due iter for this event, or END_EVENT */
  double next_time; /**< next due time for this event, or +INF */
  int index;        /**< for list schedules */
} EventRuntime;

enum
{
  EXPR_UNKNOWN = -2,
  EXPR_NONE = -1,
  EXPR_INIT = 0,
  EXPR_CONDITION = 1,
  EXPR_INCREMENT = 2,
}; // ExpressionRole;

enum
{
  SCH_IRREGULAR = 0,
  SCH_ONCE_ITER,
  SCH_ONCE_TIME,
  SCH_EVERY_ITER,
  SCH_EVERY_TIME,
  SCH_LIST_ITER,
  SCH_LIST_TIME,
}; // ScheduleType

typedef struct
{
  int type;
  union
  {
    struct
    {
      int at;
    } once_i;
    struct
    {
      double at;
    } once_t;

    struct
    {
      int start;
      int step;
    } every_i;
    struct
    {
      double start;
      double step;
    } every_t;

    struct
    {
      const int *vals;
      int n;
    } list_i;
    struct
    {
      const double *vals;
      int n;
    } list_t;
  } u;
} EventSchedule;

typedef struct _Event Event;

typedef int (*ActionFn)(int iter, double t, Event *event);
typedef bool (*CondFn)(int *iter, double *t, void *ctx);
typedef int (*Expr)(int *, double *, Event *);

struct _Event
{

  // Mandatory members imposed by the qcc abstract syntax tree using
  // them in initialization lists of register_event
  int last, nexpr;
  int (*action)(const int, const double, Event *);
  Expr expr[3];
  int *arrayi;
  double *arrayt;
  const char *file;
  int line;
  const char *name;

  // Internal members used by the original events system
  // double t;
  // int i, a;
  void *data;
  // Event* next;

  // Our own members
  ActionFn actionfn; /**< callback */
  // CondFn cond[3];      /**< optional condition */
  int expr_type[3]; /**< optional condition types */
  int expr_is_iter[3];
  int expr_is_time[3];
  EventSchedule sched; /**< schedule definition (static) */
  EventRuntime rt;     /**< runtime state (mutable, checkpointed) */
  Event *next;
};

// /* ============================
//  * Global scheduler variables (if you keep them global)
//  * ============================ */

static Event *Events = NULL; // head of linked list of head-events

int iter = 0, inext = 0;
double t = 0.0, tnext = 0.0;

static int END_EVENT = 1234567890;
static double TEND_EVENT = 1234567890;
static double TEPS = 1e-9;

// Basilisk interface(s)
static void _init_solver(void);

void event_register(Event event);
static void event_init(Event *event);
static Event *event_clone_shallow(const Event *event);
static int event_finished(Event *event);
static int event_cond(Event *event, int i, double t);
static void event_error(Event *event, const char *s);
static void event_print(Event *event, FILE *fp);
static bool event_due(const Event *event, EventCursor c, double eps);
static int event_advance(Event *event);

// My events
static int event_classify_expression(Expr expression, Event *event);
static bool event_infer_increment_iter(Expr increment_expr, Event *event, int *step);
static bool event_infer_increment_time(Expr increment_expr, Event *event, double *step);

static int events_run(bool do_action, double eps);

static int scheduler_run_due(Event *events, int n, EventCursor c, double eps);
static EventNext scheduler_next(Event *events, int n, EventCursor c);

/**
 * @brief The interpreter [overloads](/ast/interpreter/overload.h) the function
 * below to control (i.e. shorten) the events loop. We do not use it here.
 */
static bool
overload_event()
{
  return true;
}

/**
 * @brief
 *
 * @param event
 * @param action
 */
static int
event_do(Event *event, bool action)
{
  return event_advance(event);
}

/**
 * @brief The main events loop that runs events, used primarly by @ref run().
 *
 * @param action Whether or not to actually call the function created by an event
 */
int events(bool action)
{
  return events_run(action, TEPS);
}

/**
 * @brief Runs all registered events that match by name and completely bypasses all event loop logic
 *
 * @param name The name of the event
 */
void event(const char *name)
{
  if (!Events)
    return;

  for (Event *e = Events;; e++)
  {
    if (!strcmp(e->name, name))
    {
      e->action(0, 0, e);
    }
    if (e->last)
      break;
  }
}

/**
 * @brief Used to set the next time step
 *
 * @param dt
 */
double
dtnext(double dt)
{
  return dt;
}

#ifndef _GTEST
void init_solver()
{
  // Events = malloc(sizeof(Event));
  // Events[0].last = 1;
  _attribute = calloc(datasize / sizeof(real), sizeof(_Attributes));
  int n = datasize / sizeof(real);
  all = (scalar *)malloc(sizeof(scalar) * (n + 1));
  baseblock = (scalar *)malloc(sizeof(scalar) * (n + 1));
  for (int i = 0; i < n; i++)
    baseblock[i].i = all[i].i = i;
  baseblock[n].i = all[n].i = -1;
#if _CADNA
  cadna_init(-1);
#endif
#if _MPI
  mpi_init();
#elif MTRACE == 1
  char *etrace = getenv("MTRACE");
  pmtrace.fp = fopen(etrace ? etrace : "mtrace", "w");
  pmtrace.fname = systrdup(etrace ? etrace : "mtrace");
#endif
}
#endif

/**
 * @brief
 *
 * @param event
 * @param message
 */
static void
event_error(Event *event, const char *message)
{
  fprintf(stderr, "%s:%d: error: %s", event->file, event->line, message);
  exit(1);
  return;
}

/**
 * @brief
 *
 * @param event
 * @param i
 * @param t
 */
static int
event_cond(Event *ev, int i, double t)
{
  return true;
}

/* ============================
 * Helpers
 * ============================ */

/**
 * @brief
 *
 * @param a
 */
static int list_i_len(const int *a)
{
  int n = 0;
  if (!a)
    return 0;
  while (a[n] >= 0)
    n++;
  return n;
}

/**
 * @brief
 *
 * @param a
 */
static int list_t_len(const double *a)
{
  int n = 0;
  if (!a)
    return 0;
  while (a[n] >= 0.0)
    n++;
  return n;
}

/**
 * @brief
 *
 * @param a
 * @param b
 * @param eps
 */
static inline double rel_time_tol(double a, double b, double eps)
{
  // robust near 0
  double scale = fmax(1.0, fmax(fabs(a), fabs(b)));
  return eps * scale;
}

/**
 * @brief
 *
 * @param a
 * @param b
 * @param eps
 */
static inline bool time_equal(double a, double b, double eps)
{
  return fabs(a - b) <= rel_time_tol(a, b, eps);
}

/**
 * @brief
 *
 * @param event
 */
static inline void runtime_finish(Event *event)
{
  event->rt.next_iter = END_EVENT;
  event->rt.next_time = TEND_EVENT;
  event->rt.index = 0;
}

/**
 * @brief
 *
 * @param time
 */
static inline bool time_is_finite(double time)
{
  return time < HUGE;
}

/**
 * @brief
 *
 * @param event
 * @param expression
 */
static int event_classify_expression(Expr expression, Event *event)
{
  int i = -END_EVENT;
  double t = -TEND_EVENT;
  expression(&i, &t, event);

  // If the function left each unchanged, assume it is intended as a condition
  if (i == -END_EVENT && t == -TEND_EVENT)
  {
    return EXPR_CONDITION;
  }

  // Idempotence test to distinguish INIT vs INC
  int i1 = i;
  double t1 = t;
  expression(&i, &t, event);
  if (i1 == i && t1 == t)
  {
    return EXPR_INIT;
  }

  // Deduce that it must be an increment
  return EXPR_INCREMENT;
}

/**
 * @brief
 *
 * @param event
 */
static void event_sort_expressions(Event *event)
{
  assert(event->nexpr <= 3);

  Expr expr_inc = NULL;
  Expr expr_init = NULL;
  Expr expr_cond = NULL;

  for (int i = 0; i < event->nexpr; i++)
  {
    switch (event->expr_type[i])
    {
    case EXPR_INIT:
      expr_init = event->expr[i];
      break;
    case EXPR_CONDITION:
      expr_cond = event->expr[i];
      break;
    case EXPR_INCREMENT:
      expr_inc = event->expr[i];
      break;
    default:
      break;
    }
  }

  event->expr[0] = expr_init;
  event->expr[1] = expr_cond;
  event->expr[2] = expr_inc;

  event->expr_type[0] = EXPR_INIT;
  event->expr_type[1] = EXPR_CONDITION;
  event->expr_type[2] = EXPR_INCREMENT;
}

/**
 * @brief This function checks EXPR_INIT and EXPR_INCREMENT expressions to see if they are time- or iteration-based
 *
 * @param expression
 * @param event
 */
static bool event_expression_mutates_iter(Expr expression, Event *event)
{
  int i1 = 1, i2 = 1;
  double t1 = 1, t2 = 1;
  switch (event_classify_expression(expression, event))
  {
  case EXPR_INIT:
  {
    i1 = -END_EVENT;
    expression(&i1, &t1, event);
    if (i1 != -END_EVENT)
      return true;
  }
  break;

  case EXPR_INCREMENT:
  {
    i1 = -END_EVENT;
    i2 = -END_EVENT;
    expression(&i1, &t1, event);
    if (i1 != i2)
      return true;
  }
  break;

  default:
  {
    return false;
  }
  break;
  }
  return false;
}

static bool event_expression_mutates_time(Expr f, Event *event)
{
  int i1 = 1, i2 = 1;
  double t1 = 1, t2 = 1;
  switch (event_classify_expression(f, event))
  {
  case EXPR_INIT:
  {
    t1 = -TEND_EVENT;
    f(&i1, &t1, event);
    if (t1 != -TEND_EVENT)
      return true;
  }
  break;

  case EXPR_INCREMENT:
  {
    t1 = -END_EVENT;
    t2 = -END_EVENT;
    f(&i1, &t1, event);
    if (t1 != t2)
      return true;
  }
  break;
  default:
  {
    return false;
  }
  break;
  }

  return false;
}

/**
 * @brief This function infers the iteration step of the INC type expression
 *
 * @param increment_expr
 * @param event
 * @param out_step
 *
 * @returns True if the iteration step was successfully inferred
 */
static bool event_infer_increment_iter(Expr increment_expr, Event *event, int *out_step)
{
  int i = 0;
  double t = 0;
  increment_expr(&i, &t, event);
  int i1 = i;
  increment_expr(&i, &t, event);
  int i2 = i;
  int step = i2 - i1;

  // verify constant for a few more increments
  for (int k = 0; k < 4; k++)
  {
    int prev = i;
    increment_expr(&i, &t, event);
    if (i - prev != step)
      return false;
  }
  *out_step = step;
  return (step > 0);
}

/**
 * @brief This function infers the time step of the INC type expression
 *
 * @param increment_expr
 * @param event
 * @param out_step
 *
 * @returns True if the time step was successfully inferred
 */
static bool event_infer_increment_time(Expr increment_expr, Event *event, double *out_step)
{
  int i = 0;
  double t = 0;
  increment_expr(&i, &t, event);
  double t1 = t;
  increment_expr(&i, &t, event);
  double t2 = t;
  double step = t2 - t1;

  for (int k = 0; k < 4; k++)
  {
    double prev = t;
    increment_expr(&i, &t, event);
    if (fabs((t - prev) - step) > 1e-12)
      return false;
  }
  *out_step = step;
  return (step > 0);
}

/**
 * @brief
 *
 * @param event
 */
static void event_init(Event *event)
{
  // SCH_LIST_ITER
  if (event->arrayi)
  {
    event->sched.type = SCH_LIST_ITER;
    event->sched.u.list_i.vals = event->arrayi;
    event->sched.u.list_i.n = list_i_len(event->arrayi);
    event->rt.next_iter = event->sched.u.list_i.vals[0];
    event->rt.next_time = TEND_EVENT;
    return;
  }

  // SCH_LIST_TIME
  if (event->arrayt)
  {
    event->sched.type = SCH_LIST_TIME;
    event->sched.u.list_t.vals = event->arrayt;
    event->sched.u.list_t.n = list_t_len(event->arrayt);
    event->rt.next_iter = END_EVENT;
    event->rt.next_time = event->sched.u.list_t.vals[0];
    return;
  }

  // Classify expresison types
  for (int i = 0; i < event->nexpr; i++)
  {
    event->expr_type[i] = event_classify_expression(event->expr[i], event);
    if (event->expr_type[i] != EXPR_UNKNOWN)
    {
      event->expr_is_iter[i] = event_expression_mutates_iter(event->expr[i], event);
      event->expr_is_time[i] = event_expression_mutates_time(event->expr[i], event);
    }
  }

  // Set EXPR_NONE for expressions we do not have
  for (int i = event->nexpr; i < 3; i++)
  {
    event->expr_type[i] = EXPR_NONE;
  }

  // Check if expressions mix time and iterations
  for (int i = 0; i < event->nexpr; i++)
  {
    if (event->expr_is_iter[i] && event->expr_is_time[i])
    {
      event->sched.type = SCH_IRREGULAR;
      return;
    }
  }

  // Begin classifying regular schedule types; assume expressions are already sorted
  event_sort_expressions(event);

  if (event->expr[0] && event->expr[2])
  {
    // SCH_EVERY_ITER
    if (event->expr_is_iter[0])
    {
      double t = 0;
      int i_start = 0;
      event->expr[0](&i_start, &t, event);
      int i_step = 0;
      bool is_regular = event_infer_increment_iter(event->expr[2], event, &i_step);

      if (is_regular)
      {
        event->sched.type = SCH_EVERY_ITER;
        event->sched.u.every_i.start = i_start;
        event->sched.u.every_i.step = i_step;
        event->rt.next_iter = i_start;
        event->rt.next_time = TEND_EVENT;
        return;
      }
      else
      {
        event->sched.type = SCH_IRREGULAR;
        return;
      }
    }

    // SCH_EVERY_TIME
    if (event->expr_is_time[0])
    {
      int i = 0;
      double t_start = 0;
      event->expr[0](&i, &t_start, event);
      double t_step = 0;
      bool is_regular = event_infer_increment_time(event->expr[2], event, &t_step);

      if (is_regular)
      {
        event->sched.type = SCH_EVERY_TIME;
        event->sched.u.every_t.start = t_start;
        event->sched.u.every_t.step = t_step;
        event->rt.next_iter = END_EVENT;
        event->rt.next_time = t_start;
        return;
      }
      else
      {
        event->sched.type = SCH_IRREGULAR;
        return;
      }
    }
  }

  if (event->expr[0])
  {
    // SCH_ONCE_ITER
    if (event->expr_is_iter[0])
    {
      int i_start = 0;
      double t = 0;
      event->expr[0](&i_start, &t, event);
      event->sched.type = SCH_ONCE_ITER;
      event->sched.u.once_i.at = i_start;
      event->rt.next_iter = i_start;
      event->rt.next_time = TEND_EVENT;
      return;
    }

    // SCH_ONCE_TIME
    if (event->expr_is_time[0])
    {
      int i = 0;
      double t_start = 0;
      event->sched.type = SCH_ONCE_TIME;
      event->expr[0](&i, &t_start, event);
      event->sched.u.once_t.at = t_start;
      event->rt.next_iter = END_EVENT;
      event->rt.next_time = t_start;
      return;
    }
  }

  // SCH_IRREGULAR
  event->sched.type = SCH_IRREGULAR;
}

/**
 * @brief
 *
 * @param event
 * @param cursor
 * @param eps
 */
static bool event_due(const Event *event, EventCursor cursor, double eps)
{
  assert(event);

  // 1. Check whether this event is due by iteration or time.
  const bool due_i = (event->rt.next_iter == cursor.iter);
  const bool due_t = time_is_finite(event->rt.next_time) && time_equal(cursor.t, event->rt.next_time, eps);

  if (!(due_i || due_t))
    return false;

  // 2. Optional condition expression (Basilisk-style "COND").
  //    If there is a condition expr[1], evaluate it; if it returns 0, skip.
  if (event->expr[1] && event->expr_type[1] == EXPR_CONDITION)
  {
    int i = cursor.iter;
    double t = cursor.t;
    int cond_result = event->expr[1](&i, &t, (Event *)event);
    if (!cond_result)
      return false;
  }

  return true;
}

/* ============================
 * advance: move runtime to next trigger
 * (this is what prevents reruns after restore)
 * ============================ */

static int event_advance(Event *event)
{
  assert(event);

  switch (event->sched.type)
  {
  case SCH_ONCE_ITER:
  case SCH_ONCE_TIME:
    runtime_finish(event);
    break;

  case SCH_EVERY_ITER:
  {
    int step = event->sched.u.every_i.step;
    assert(step > 0);
    if (event->rt.next_iter == END_EVENT)
    {
      // should not happen unless someone finished it manually
      event->rt.next_iter = event->sched.u.every_i.start;
    }
    else
    {
      event->rt.next_iter += step;
    }
  }
  break;

  case SCH_EVERY_TIME:
  {
    double step = event->sched.u.every_t.step;
    assert(step > 0.0);
    if (!time_is_finite(event->rt.next_time))
    {
      event->rt.next_time = event->sched.u.every_t.start;
    }
    else
    {
      event->rt.next_time += step;
    }
  }
  break;

  case SCH_LIST_ITER:
    event->rt.index++;
    if (event->rt.index >= event->sched.u.list_i.n)
    {
      event->rt.next_iter = END_EVENT;
    }
    else
    {
      event->rt.next_iter = event->sched.u.list_i.vals[event->rt.index];
    }
    break;

  case SCH_LIST_TIME:
    event->rt.index++;
    if (event->rt.index >= event->sched.u.list_t.n)
    {
      event->rt.next_time = TEND_EVENT;
    }
    else
    {
      event->rt.next_time = event->sched.u.list_t.vals[event->rt.index];
    }
    break;
  }

  return 0;
}

/* ============================
 * run: execute all due events exactly once at cursor
 * ============================ */
static int scheduler_run_due(Event *events, int n, EventCursor c, double eps)
{
  int stop = 0;

  for (int k = 0; k < n; k++)
  {
    Event *ev = &events[k];
    if (!event_due(ev, c, eps))
      continue;

    int r = ev->action(c.iter, c.t, ev);
    if (r)
      stop = 1;

    event_advance(ev);
  }

  return stop;
}

/* ============================
 * next: compute global next trigger (derived)
 * ============================ */

static EventNext scheduler_next(Event *events, int n, EventCursor cursor)
{
  EventNext nx = {.next_iter = END_EVENT, .next_time = TEND_EVENT};

  for (int k = 0; k < n; k++)
  {
    Event *event = &events[k];

    if (event->rt.next_iter > cursor.iter && event->rt.next_iter < nx.next_iter)
      nx.next_iter = event->rt.next_iter;

    if (time_is_finite(event->rt.next_time) && event->rt.next_time > cursor.t &&
        event->rt.next_time < nx.next_time)
      nx.next_time = event->rt.next_time;
  }

  return nx;
}

/* ============================
 * Linked-list registration (optional)
 * If you want an array, store them in a vector/array and pass n.
 * ============================ */

static Event *event_clone_shallow(const Event *src)
{
  Event *shallow_copy = (Event *)calloc(1, sizeof(Event));
  assert(shallow_copy);
  *shallow_copy = *src;
  shallow_copy->next = NULL;
  return shallow_copy;
}

static void events_fix_last_flags(void)
{
  if (!Events)
    return;

  // clear all
  for (Event *e = Events; e; e = e->next)
    e->last = 0;

  // set tail
  Event *tail = Events;
  while (tail->next)
    tail = tail->next;
  tail->last = 1;
}
void event_register(Event event)
{
  assert(event.nexpr <= 3);

  // initialize schedule/runtime
  event_init(&event);

  // count existing events
  int n_existing = 0;
  if (Events)
  {
    for (Event *e = Events; !e->last; e++)
      n_existing++;
    n_existing++; // include the last one itself
  }

  // allocate new array (+1 new event + 1 sentinel)
  Event *new_arr = (Event *)calloc((size_t)(n_existing + 1), sizeof(Event));

  // copy old events if any
  if (Events)
  {
    memcpy(new_arr, Events, n_existing * sizeof(Event));
    free(Events);
  }

  // append the new event
  new_arr[n_existing - 1].last = 0; // ensure previous last is cleared
  new_arr[n_existing] = event;
  new_arr[n_existing].last = 1;

  // replace global pointer
  Events = new_arr;
}

/* ============================
 * Convenience wrapper using globals (like Basilisk)
 * ============================ */
static int events_run(bool do_action, double eps)
{
  if (!Events)
    return 0;

  // Count how many events until .last == 1
  int n = 0;
  for (Event *e = Events;; e++)
  {
    n++;
    if (e->last)
      break;
  }

  int stop = 0;
  EventCursor c = {.iter = iter, .t = t};

  if (do_action)
  {
    for (int k = 0; k < n; k++)
    {
      Event *ev = &Events[k];
      if (!event_due(ev, c, eps))
        continue;

      int r = ev->action(c.iter, c.t, ev);
      if (r)
        stop = 1;

      event_advance(ev);
    }
  }

  // Compute global next trigger
  inext = END_EVENT;
  tnext = TEND_EVENT;

  for (int k = 0; k < n; k++)
  {
    Event *ev = &Events[k];

    if (ev->rt.next_iter > iter && ev->rt.next_iter < inext)
      inext = ev->rt.next_iter;

    if (time_is_finite(ev->rt.next_time) && ev->rt.next_time > t && ev->rt.next_time < tnext)
      tnext = ev->rt.next_time;
  }

  return stop ? 0 : 1;
}
