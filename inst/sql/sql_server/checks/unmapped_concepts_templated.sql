-- top 25 unmapped

select
	ROW_NUMBER() OVER(ORDER BY num_records desc) as row_num,
	source_value as source_value,
  CAST(source_concept_id AS VARCHAR) as source_concept_id,
  concept.concept_name as source_concept_name,
	num_records as n_records,
	100.0 * num_records/t.total_records as p_records
from #@cdmDomain as cte
cross join (select sum(num_records) as total_records from #@cdmDomain) t
left join @cdmDatabaseSchema.concept as concept on cte.source_concept_id = concept.concept_id
where is_mapped = 0 and num_records > @smallCellCount
order by num_records desc
;
