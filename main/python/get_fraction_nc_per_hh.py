import argparse
import csv
import os
import xml.etree.ElementTree as ET

def main(population_dir, population_name):
    # Get exceedance probability per NIS code
    exceedance_prob_file = "exprob_R6_BE_PC_1.txt"

    exceedance_prob_by_nis = {}
    with open(exceedance_prob_file) as f:
        # Skip header
        next(f)
        for line in f:
            line = line.split()
            nis_code = int(line[2])
            exprob = float(line[0])
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

    # Match households to district
    exceedance_prob_by_hh = {}
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_household_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_id = int(row["district_id"])
            exceedanc_prob = exceedance_prob_by_district[district_id]
            hh_id = int(row["hh_id"])
            exceedance_prob_by_hh[hh_id] = exceedanc_prob

    # Write household ids to file
    root = ET.Element("hotspots")
    for hh_id, exceedanc_prob in exceedance_prob_by_hh.items():
        new_hh = ET.SubElement(root, "hotspot")
        new_hh_id = ET.SubElement(new_hh, "id")
        new_hh_id.text = str(hh_id)
        new_hh_fraction_non_compliers = ET.SubElement(new_hh, "fraction_non_compliers")
        new_hh_fraction_non_compliers.text = str(exceedanc_prob)
    ET.ElementTree(root).write(population_name + "_non_compliers_by_exceedance_prob.xml")

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--population_dir", type=str, default=".")
    parser.add_argument("--population_name", type=str, default="pop_belgium3000k_c500_teachers_censushh")
    args = parser.parse_args()
    main(args.population_dir, args.population_name)
