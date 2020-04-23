import argparse
import csv
import os
import xml.etree.ElementTree as ET

def main(population_dir, population_name, fraction_non_compliers):
    # Get NIS codes in hotspots
    exceedance_prob_file = "exprob_R6_BE_PC_1.txt"
    exceedance_prob_limit = 0.9

    hotspot_nis_codes = []
    with open(exceedance_prob_file) as f:
        # Skip header
        next(f)
        for line in f:
            line = line.split()
            exprob = float(line[0])
            if exprob > exceedance_prob_limit:
                hotspot_nis_codes.append(int(line[2]))

    # Remove doubles
    hotspot_nis_codes = list(set(hotspot_nis_codes))
    # Match NIS codes to district IDs in the simulator
    hotspot_district_ids = []
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_district_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_nis_code = int(row["city"])
            if district_nis_code in hotspot_nis_codes:
                district_id = int(row["id"])
                hotspot_district_ids.append(district_id)

    # Find households that are in 'hotspot' districts
    households_in_hotspots = []
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_household_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_id = int(row["district_id"])
            if district_id in hotspot_district_ids:
                hh_id = int(row["hh_id"])
                households_in_hotspots.append(hh_id)

    # Write household ids to file
    root = ET.Element("hotspots")
    for hh_id in households_in_hotspots:
        new_hh = ET.SubElement(root, "hotspot")
        new_hh_id = ET.SubElement(new_hh, "id")
        new_hh_id.text = str(hh_id)
        new_hh_fraction_non_compliers = ET.SubElement(new_hh, "fraction_non_compliers")
        new_hh_fraction_non_compliers.text = str(fraction_non_compliers)

    ET.ElementTree(root).write(population_name + "_households_in_hotspots_fraction_nc_" + str(fraction_non_compliers) + ".xml")


if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--population_dir", type=str, default=".")
    parser.add_argument("--population_name", type=str, default="pop_belgium3000k_c500_teachers_censushh")
    parser.add_argument("--fraction_non_compliers", type=float, default=1)
    args = parser.parse_args()
    main(args.population_dir, args.population_name, args.fraction_non_compliers)
