mdd_t* mdd_minmax(mdd_manager* mgr, mdd_t* mdd, int id, int max);
mdd_t* mdd_incr(mdd_manager* mgr, mdd_t* mdd, int id);

#define mdd_max(mgr,m,id) mdd_minmax((mgr), (m), (id), 1)
#define mdd_min(mgr,m,id) mdd_minmax((mgr), (m), (id), 0)
