import argparse
import csv
import os
import xml.etree.ElementTree as ET

def main(population_dir, population_name):
    # Postcodes of municipalities that were hotspots for at least one flu-like symptom
    # (covid-symptom) in the analysis of Survey 4 (14/04) (exceedance probability > 0.9)
    hotspot_zipcodes = [2460, 3010, 3270, 3290, 3300, 3404, 3440, 3450, 3454, 3460, 3470, 3500, 3501, \
                            3510, 3511, 3512, 3520, 3530, 3540, 3545, 3550, 3560, 3570, 3580, 3581, \
                            3582, 3583, 3590, 3600, 3620, 3621, 3630, 3631, 3640, 3650, 3660, 3665, \
                            3668, 3670, 3680, 3690, 3700, 3720, 3721, 3722, 3723, 3724, 3730, 3740, \
                            3770, 3800, 3803, 3806, 3830, 3831, 3832, 3840, 3850, 3870, 3890, 3891, \
                            3900, 3910, 3920, 3930, 3940, 3941, 3950, 3960, 3970, 3980, 3990, 4000, \
                            4300, 4340, 4347, 4360, 4367, 4452, 4690]
    hotspot_districts = {}
    for zipcode in hotspot_zipcodes:
        hotspot_districts[zipcode] = {
            "district_name": None,
            "district_nis_code": None,
            "district_id": None
        }

    # Match zipcodes to district names
    # Based on list downloaded via https://www.bpost.be/site/nl/verzenden/adressering/zoek-een-postcode
    # (21/04/2020)
    with open(os.path.join("..", "resources", "data", "zipcodes_BE.csv"), encoding="utf-8-sig") as csvfile:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            zipcode = int(row["Postcode"])
            if zipcode in hotspot_zipcodes:
                district_name = row["Hoofdgemeente"].upper().strip()
                hotspot_districts[zipcode]["district_name"] = district_name

    # Match district names to NIS codes
    # Based on 2019 list downloaded via https://statbel.fgov.be/nl/over-statbel/methodologie/classificaties/geografie
    # (21/04/2020)
    with open(os.path.join("..", "resources", "data", "REFNIS_2019.csv")) as csvfile:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            district_name = row["Administratieve eenheden"].upper().strip()
            for zipcode, info in hotspot_districts.items():
                if info["district_name"] == district_name:
                    district_nis_code = int(row["Code NIS"])
                    hotspot_districts[zipcode]["district_nis_code"] = district_nis_code

    # Match NIS codes to district ID in simulator
    with open(os.path.join(population_dir, population_name + "_all", population_name + "_district_data.csv")) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            district_nis_code = int(row["city"])
            for zipcode, info in hotspot_districts.items():
                if info["district_nis_code"] == district_nis_code:
                    district_id = int(row["id"])
                    hotspot_districts[zipcode]["district_id"] = district_id

    hotspot_district_ids = [x["district_id"] for x in list(hotspot_districts.values())]
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
        new_hh = ET.SubElement(root, "id")
        new_hh.text = str(hh_id)

    ET.ElementTree(root).write(population_name + "_households_in_hotspots.xml")

if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--population_dir", type=str, default=".")
    parser.add_argument("--population_name", type=str, default="pop_belgium3000k_c500_teachers_censushh")
    args = parser.parse_args()
    main(args.population_dir, args.population_name)
