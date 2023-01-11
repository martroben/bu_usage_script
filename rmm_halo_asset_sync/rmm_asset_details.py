
import re
import requests
import json
import xml.etree.ElementTree as xml
from tqdm import tqdm
from dotenv import dotenv_values


class Asset:
    def add_attributes_from_dict(self, dictionary):
        for key, value in dictionary.items():
            setattr(self, key, value)

    def __str__(self):
        return f"{self.id} | {self.client} | {self.name}"


def get_clients():

    parameters = {
        "apikey": RMM_API_KEY,
        "service": "list_clients"}

    response = requests.get(RMM_BASE_URL, params=parameters)
    response_xml = xml.fromstring(response.text)

    clients = dict()
    for client in response_xml.findall("./items/client"):
        clients[client.find("./name").text] = client.find("./clientid").text

    return clients


def get_assets_at_client(client_id, asset_type):

    request_parameters = {
        "apikey": RMM_API_KEY,
        "service": "list_devices_at_client",
        "clientid": client_id,
        "devicetype": asset_type}

    response = requests.get(RMM_BASE_URL, params=request_parameters)
    response_xml = xml.fromstring(response.text)

    assets = list()
    for site_xml in response_xml.findall("./items/client/site"):
        for asset_xml in site_xml.findall(f"./{asset_type.lower()}"):
            try:
                asset = Asset()
                asset.client = response_xml.find("./items/client/name").text
                asset.site = site_xml.find("name").text
                asset.id = asset_xml.find("id").text
                asset.name = asset_xml.find("name").text
                # Mobile devices don't have description attribute
                asset.description = str() if asset_type == "mobile_device" else asset_xml.find("description").text
                asset.type = asset_type
                assets += [asset]
            except Exception as exception:
                log_string = f"{type(exception).__name__} happened while getting client assets. " \
                             f"Client_id: {client_id}, asset_type: {asset_type}. {exception}"
                print(log_string)
                del log_string
    return assets


def parse_role(role_id: str) -> str:
    """
    https://documentation.n-able.com/remote-management/userguide/Content/listing_device_asset_details.htm
    :param role_id:
    :return:
    """
    role_reference = {
        "0": "Windows workstation",
        "1": "Windows workstation",
        "2": "Windows server",
        "3": "Windows server",
        "4": "Windows server",
        "5": "Windows server",
        "101": "Unix/Linux",
        "102": "Mac",
        "103": "printer",
        "104": "router",
        "105": "NAS",
        "106": "other network device"}

    if role_id in role_reference:
        return role_reference[role_id]
    return "unknown"


def get_asset_details(asset_id):
    asset = dict()
    request_parameters = {
        "apikey": RMM_API_KEY,
        "service": "list_device_asset_details",
        "deviceid": asset_id}

    response = requests.get(RMM_BASE_URL, params=request_parameters)
    response_xml = xml.fromstring(response.text)

    # Define asset type as laptop if chassis type is 8-11 or 14
    chassis_type = response_xml.find("./chassistype").text
    if chassis_type in [8, 9, 10, 11, 14]:
        asset["type"] = "laptop"

    # json of different ip-s
    ips = {f"ip{i + 1}": ip_xml.text for i, ip_xml in enumerate(response_xml.findall("./ip"))}
    asset["ip"] = json.dumps(ips)

    # json of different mac addresses
    mac_fields = ["mac1", "mac2", "mac3"]
    macs = {mac_field: response_xml.find(f"./{mac_field}").text for mac_field in mac_fields}
    asset["mac"] = json.dumps(macs)

    asset["user"] = response_xml.find("./user").text
    asset["manufacturer"] = response_xml.find("./manufacturer").text
    asset["model"] = response_xml.find("./model").text
    asset["os"] = response_xml.find("./os").text

    # Parse only the date string from os install date and time
    date_pattern = re.compile(r"\d{4}-\d{2}-\d{2}")
    osinstalldate = response_xml.find("./osinstalldate").text
    asset["os_install_date"] = date_pattern.findall(osinstalldate)[0] if osinstalldate else None

    asset["serial_number"] = response_xml.find("./serialnumber").text
    asset["product_key"] = response_xml.find("./productkey").text

    role = response_xml.find("./role").text
    asset["device_role"] = parse_role(role)

    ram = response_xml.find("./ram").text
    asset["ram_gb"] = float(ram) / 2 ** 10 / 2 ** 10 / 2 ** 10 if ram else None

    # Make a dict of custom fields {fieldname: value}
    custom_fields_xml = [response_xml.find(f"./custom{i}") for i in range(11)]
    custom_fields = {element.attrib["customname"]: element.text for element in custom_fields_xml if element is not None}
    # convert to json format
    asset["custom_fields"] = json.dumps(custom_fields)

    return asset


################################
# Load environmental variables #
################################

env_file_path = ".env"
# dotenv_values just reads from file without assigning variables to environment.
# use load_dotenv for the "real thing"
env_variables = dotenv_values(env_file_path)
RMM_BASE_URL = env_variables["RMM_BASE_URL"]
RMM_API_KEY = env_variables["RMM_API_KEY"]


###########
# Execute #
###########

clients = get_clients()

assets = list()
for client_id in clients.values():
    for asset_type in ["server", "workstation", "mobile_device"]:
        assets += get_assets_at_client(client_id=client_id, asset_type=asset_type)

assets_with_details = list()
for asset in tqdm(assets, desc="Getting asset details..."):
    if asset.type == "mobile_device":
        assets_with_details += [asset]
        continue
    asset.add_attributes_from_dict(get_asset_details(asset.id))
    assets_with_details += [asset]

for asset in assets_with_details:
    print(asset)
