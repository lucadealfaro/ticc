mdd_t* mdd_minmax(mdd_t* mdd, int id, int max);
mdd_t* mdd_incr(mdd_manager* mgr, mdd_t* mdd, int id);
int mdd_get_unique_value(mdd_manager* mgr, mdd_t* mdd, int id);

#define mdd_max(m,id) mdd_minmax((m), (id), 1)
#define mdd_min(m,id) mdd_minmax((m), (id), 0)
