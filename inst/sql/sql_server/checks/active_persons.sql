select count(distinct person_id)
from @cdmDatabaseSchema.observation_period
cross join @cdmDatabaseSchema.cdm_source
where observation_period_end_date >= DATEADD(month, -6, coalesce(source_release_date, cdm_release_date))
;
