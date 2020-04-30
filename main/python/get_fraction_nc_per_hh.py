import argparse
import csv
import os
import xml.etree.ElementTree as ET

def main(population_dir, population_name, use_pct_of_exprob):
    # Get exceedance probability per NIS code
    exceedance_prob_file = "exprob_R6_BE_PC_1.txt"

    exceedance_prob_by_nis = {}
    with open(exceedance_prob_file) as f:
        # Skip header
        next(f)
        for line in f:
            line = line.split()
            nis_code = int(line[2])
            exprob = float(line[0]) * (use_pct_of_exprob / 100)
            if nis_code in exceedance_prob_by_nis:
                exceedance_prob_by_nis[nis_code].append(exprob)
            else:
                exceedance_prob_by_nis[nis_code] = [exprob]

    for nis_code, exprobs in exceedance_prob_by_nis.items():
        avg_exprob = sum(exprobs) / len(exprobs)
        exceedance_prob_by_nis[nis_code] = avg_exprob

    exceedance_prob_by_district = {}
    # Match NIS codes to district IDs in the simulator
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_district_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_nis_code = int(row["city"])
            exceedanc_prob = exceedance_prob_by_nis[district_nis_code]
            district_id = int(row["id"])
            exceedance_prob_by_district[district_id] = exceedanc_prob

    # Match households to districts
    households_by_district = {}
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_household_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_id = int(row["district_id"])
            hh_id = int(row["hh_id"])
            if district_id in households_by_district:
                households_by_district[district_id].append(hh_id)
            else:
                households_by_district[district_id] = [hh_id]

    # Write to file
    root = ET.Element("hotspots")
    for district_id in households_by_district.keys():
        new_district = ET.SubElement(root, "district")
        new_district_id = ET.SubElement(new_district, "id")
        new_district_id.text = str(district_id)
        new_district_fraction_nc = ET.SubElement(new_district, "fraction_non_compliers")
        new_district_fraction_nc.text = str(exceedance_prob_by_district[district_id])
        new_households = ET.SubElement(new_district, "households")
        for hh_id in households_by_district[district_id]:
            new_hh_id = ET.SubElement(new_households, "hh_id")
            new_hh_id.text = str(hh_id)

    ET.ElementTree(root).write(population_name + "_non_compliers_by_exceedance_prob_" + str(use_pct_of_exprob) + ".xml")

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--population_dir", type=str, default=".")
    parser.add_argument("--population_name", type=str, default="pop_belgium3000k_c500_teachers_censushh")
    parser.add_argument("--use_pct_of_exprob", type=float, default=100)
    args = parser.parse_args()
    main(args.population_dir, args.population_name, args.use_pct_of_exprob)
