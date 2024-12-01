from openai import OpenAI
import json
import re
import os
import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'


def fetch_context(file, context):
    with open(file) as f:
        s = json.load(f)
    return s[context]

def extract_answers(answers):
    answers_dict = {}
    for q in ["p1", "p2", "p3", "p4", "p5", "p6", "e4", "e5", "e6", "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"]:
        try:
            answers_dict[q] = extract_answer(answers, q)
        except:
            answers_dict[q] = None

    try:
        e1 = extract_answer(answers, "e1")
        answers_dict["e1_currentpolicy"] = e1["current_policy"]
        answers_dict["e1_newpolicy"] = e1["new_policy"]
    except:
        answers_dict["e1_currentpolicy"] = None
        answers_dict["e1_newpolicy"] = None

    try:
        e2 = extract_answer(answers, "e2")
        for k in ["a", "b", "c", "d", "e", "f"]:
            answers_dict["e2_"+k] = e2[k]
    except:
        for k in ["a", "b", "c", "d", "e", "f"]:
            answers_dict["e2_"+k] = None

    try:
        e3 = extract_answer(answers, "e3")
        # answers_dict["e3"] = int(e3["vote_for"])
        answers_dict["e3"] = e3["vote_for"]
        # answers_dict["e3"] = extract_answer(answers,"e3") 
    except:
        answers_dict["e3"] = None

    try:
        e7 = extract_answer(answers, "e7")
        answers_dict["e7"] = e7["vote"]
    except:
        answers_dict["e7"] = None

    return answers_dict

def extract_answer(a, q):
    answer = ""
    
    if q in ["p1", "p2"]:
        #x = re.search(r"\"Answer\": .*(\d+[\.\d+]?).*", answers[question])
        if type(a["answer"]) in [float, int]:
            answer = a["answer"]
        else:
            x = re.search(r"[^0-9]*(\d+\.?\d*)[^0-9]*", a["answer"])
            if len(x.groups()) != 1:
                raise Exception("Error extracting answer for question " + q)
            answer = float(x.groups()[0])

    if q in ["e1"]:
        if type(a["current policy"]) == int:
            current_policy = a["current policy"]
        else:
            x = re.search(r"\s?[^0-9]*([1-5])[^0-9]*", a["current policy"])
            if len(x.groups()) != 1:
                raise Exception("Error extracting answer for question " + q)
            current_policy = int(x.groups()[0])

        if type(a["new policy"]) == int:
            new_policy = a["new policy"]
        else:
            y = re.search(r"\s?[^0-9]*([1-5])[^0-9]*", a["new policy"])
            if len(y.groups()) != 1:
                raise Exception("Error extracting answer for question " + q)
            new_policy = int(y.groups()[0])

        answer = {"current_policy": current_policy, "new_policy": new_policy}

    if q in ["e2", "d2"]:
        answer = {}
        for k in ["a", "b", "c", "d", "e", "f", "g", "h"]:
            if type(a[k]) == int:
                answer[k] = a[k]
            else:
                x = re.search(r"\s?[^0-9]*([1-5])[^0-9]*", a[k])
                if len(x.groups()) != 1:
                    raise Exception("Error extracting answer for question " + q)
                answer[k] = int(x.groups()[0])
            if answer[k] < 1 or answer[k] > 5:
                raise Exception("Answer not in bounds.")

    if q in ["e3"]:
        x = re.search(r".*(Yes|No|No opinion).*", a["answer"])
        # x = re.search(r"\"Answer\": (.*)", answers[question])
        if len(x.groups()) != 1:
            raise Exception("Error extracting answer for question " + q)
        answer = x.groups()[0]
        # vote_for = x.groups()[0]
        #vote_for = "Against" 
        #if x.groups()[0] in ["Yes", "yes"]:
        #    vote_for = "In Favor"
        #if x.groups()[0] in ["Abstain", "abstain"]:
        #    vote_for = "Abstain"
        #answer = {"vote_for": vote_for}
    
    if q in ["e4", "e5", "e6", "q9"]:
        answer = a["answer"]

    if q in ["e7"]:
        x = re.search(r".*(In Favor|Against|Follow|Abstain).*", a["answer"])
        if len(x.groups()) != 1:
            raise Exception("Error extracting answer for question " + q)
        answer = x.groups()[0]

    if q in ["d1"]:
        if type(a["answer"]) == int:
            answer = a["answer"]
        else:
            x = re.search(r"\s?[^0-9]*([0-9]*)[^0-9]*", a["answer"])
            if len(x.groups()) != 1:
                raise Exception("Error extracting answer for question " + q)
            answer = int(x.groups()[0])
        if answer < 1 or answer > 10:
            raise Exception("Answer not in bounds.")

    if q in ["q1", "q2", "q3", "q4", "q5", "q7", "q10"]:
        if a["answer"] not in ['A', 'B', 'C', 'D']:
            raise Exception("Error extracting answer for question " + q)
        answer = a["answer"]

    if q in ["q6"]:
        if a["answer"] not in ['A', 'B', 'C', 'D', 'E']:
            raise Exception("Error extracting answer for question " + q)
        answer = a["answer"]

    if q in ["q8"]:
        if a["answer"] not in ['A', 'B', 'C']:
            raise Exception("Error extracting answer for question " + q)
        answer = a["answer"]

    return answer

def get_question(file, nb):
    # Add a question to the current chat
    with open(file) as f:
        s = json.load(f)
    q = [q for q in s["questions"] if q["question_nb"]==nb]
    if len(q) != 1:
        raise Exception("Cannot find the survey question")
    q = q[0]

    # messages += [{'role': 'user', 'content': q["question"]}]

    pre_prompt = q["prompt"]

    if q["question_type"] == "MCQ":
        pre_prompt = "Choose one of the following answers:\n" + "\n".join(q["choices"])
        # pre_prompt = "\nChoose one of the following answers:\n" + "\n".join([str(c[0]) + ": " + c[1] for c in zip([chr(ord('@')+i) for i in range(1, len(q["choices"])+1)], q["choices"])])
        # messages += [{'role': 'user', 'content': prompt_mcq}]

    # return messages + [{'role': 'user', 'content': q["question"] + "\n" + pre_prompt + "\n" + q["prompt"]}]
    return [{'role': 'user', 'content': q["question"] + "\n" + pre_prompt + "\n" + q["prompt"]}]

def get_answer(messages, file, question, model, seed, client, trial=1, gemini=False, temperature=None):
    if trial > 10:
        raise Exception("Question " + question + " has been tried more than 10 times.")

    try:
        if gemini:
            response = client.chat.completions.create(
                    model=model,
                    messages= messages + get_question(file, question),
                    stream=False,
                    max_tokens=200,
                    temperature=temperature)
            # Extract the json object
            x = re.search(r"json(\{.*\})", response.choices[0].message.content.replace('\n', ''))
            if len(x.groups()) != 1:
                raise Exception("Error extracting json from response.")
            answer = json.loads(x.groups()[0])
        else:
            response = client.chat.completions.create(
                    model=model,
                    messages= messages + get_question(file, question),
                    stream=False,
                    max_tokens=200,
                    response_format = {"type": "json_object"},
                    temperature=temperature,
                    seed=seed)
            # Parse response
            # print(response.choices[0].message.content)
            answer = json.loads(response.choices[0].message.content)
        #print(answer)
        answer_extracted = extract_answer(answer, question)
        #print(answer_extracted)
        # If no error
        messages += get_question(file, question) + [{'role': response.choices[0].message.role, 'content': response.choices[0].message.content}]
        # print('Trials: ' + str(trial))
        return answer_extracted
    except Exception as e:
        print("Exception: " + str(e))
        print("Extra trial for question "+question)
        return get_answer(messages, file, question, model, seed, client, trial+1, gemini=gemini)

def run_survey(url="http://localhost:11434/v1", api_key="-", model="llama3.2:1b", survey="survey_overreporting.json", seed=None, i=1, gemini=False, temperature=None):

    client = OpenAI(
        base_url=url,
        api_key=api_key
    )

    messages = []
    answers = {}
    response = None

    try:
        # Preliminary questions
        messages += [{"role": "system", "content": fetch_context(survey, "context_intro")}]
        messages += [{"role": "system", "content": fetch_context(survey, "context_general_1")}]
        messages += [{"role": "system", "content": fetch_context(survey, "context_general_2")}]
        for q in [
                "p1", "p2",
                "e1", "e2", "e3", "e4", "e5", "e6", "e7",
                "d1", "d2",
                "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"
                ]:
            #print(q, end="\r")
            answers[q] = get_answer(messages, survey, q, model, seed, client, gemini=gemini, temperature=temperature)

        # Save
        survey_name = survey.replace(".json", "")
        file_messages = 'surveys/messages_'+survey_name+"_"+model+'_id_'+str(i)+'_seed_'+str(seed)+'.json'
        if temperature is not None:
            file_messages = 'surveys/messages_'+survey_name+"_"+model+'_id_'+str(i)+'_seed_'+str(seed)+'_temp_'+str(temperature)+'.json'
        file_answers = 'surveys/answers_'+survey_name+"_"+model+'_id_'+str(i)+'_seed_'+str(seed)+'.json'
        if temperature is not None:
            file_answers = 'surveys/answers_'+survey_name+"_"+model+'_id_'+str(i)+'_seed_'+str(seed)+'_temp_'+str(temperature)+'.json'
        with open(file_messages, 'w') as f:
            json.dump(messages, f)
        with open(file_answers, 'w') as f:
            json.dump(answers, f)

        return messages, answers, response
    except Exception as e:
        print("Exception: " + str(e))
        print("Extra trial for survey.")
        run_survey(url, api_key, model, survey, seed, i, gemini, temperature)

def run_surveys(url="http://localhost:11434/v1", api_key="-", model="llama3.2:3b", n=10, seeded=False, gemini=False, temperature=None):
        
    seed = None

    for i in range(0,n):
        if seeded:
            seed = i
        print(f"{model}: Survey over-reporting ({i+1}/{n}) with seed {seed}, temperature {temperature}")
        run_survey(url=url, api_key=api_key, model=model, survey="survey_overreporting.json", seed=seed, i=i+1, gemini=gemini, temperature=temperature)
        print(f"{model}: Survey under-reporting ({i+1}/{n}) with seed {seed}, temperature {temperature}")
        run_survey(url=url, api_key=api_key, model=model, survey="survey_underreporting.json", seed=seed, i=i+1, gemini=gemini, temperature=temperature)
    
# Functions for running all models

models_local = [
        #"llama3.2:1b",
        # "llama3.2:3b",
        # "gemma2:2b",
        "mistral:7b",
        # "phi3.5:3.8b",
        "llama3.1:8b",
        # "gemma2:9b",
        # "gemma2:27b",
        # "llama3.1:70b"
        ]

#models_chatgpt = ["gpt-4o", "gpt-4o-mini", "gpt-4-turbo", "gpt-4"]
models_chatgpt = [
        "gpt-4o",
        # "gpt-4o-mini"
        ]

models_gemini = [
        "gemini-1.5-flash-002",
        # "gemini-1.5-flash-8b",
        ]

def run_all_surveys_local(url="http://localhost:11434/v1", n=100, seeded=False, temperature=None):
    # URL could be one of
    # - http://localhost:11434/v1
    # - http://host.docker.internal:11434/v1

    api_key = "-"
    os.environ["OPENAI_API_KEY"] = api_key


    for m in models_local:
        run_surveys(url=url, api_key=api_key, model=m, n=n, seeded=seeded, temperature=temperature)

def run_all_surveys_chatgpt(n=100, seeded=False, temperature=None):
    # OpenAI endpoint
    url = "https://api.openai.com/v1"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("OPENAI_API_KEY")
    if api_key is None:
        try:
            with open("OPENAI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No OpenAI API key found.")

    for m in models_chatgpt:
        run_surveys(url=url, api_key=api_key, model=m, n=n, seeded=seeded, temperature=temperature)

def run_all_surveys_gemini(n=100, seeded=False, temperature=None):
    # OpenAI endpoint
    url = "https://generativelanguage.googleapis.com/v1beta/openai/"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("GEMINI_API_KEY")
    if api_key is None:
        try:
            with open("GEMINI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No Gemini API key found.")
    os.environ["OPENAI_API_KEY"] = api_key

    for m in models_gemini:
        run_surveys(url=url, api_key=api_key, model=m, n=n, seeded=seeded, gemini=True, temperature=temperature)

# Functions for textual analysis

# 1. For explanations regarding recommendation

def run_human_textual_analysis_recommendation(file="../human_survey/data.csv"):
    # Uses gpt-4o for the textual analysis
    url = "https://api.openai.com/v1"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("OPENAI_API_KEY")
    if api_key is None:
        try:
            with open("OPENAI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No OpenAI API key found.")

    client = OpenAI(
        base_url=url,
        api_key=api_key
    )

    df = pd.read_csv(file)
    df = df[["id", "Q13", "Q14", "Q13.1", "Q14.1"]][8:174]
    answers = []
    for i in range(0, len(df)):
        print("Textual Analyses for Humans (" + str(i+1) + " / " + str(len(df)) + ")", end="\r")
        r = df.iloc[i, :]
        rec = r["Q13"]
        statement = r["Q14"]
        if isinstance(statement, float):
            rec = r["Q13.1"]
            statement = r["Q14.1"]
        if isinstance(statement, str):
            context = "For context, a board member recommended to change an existing reporting policy."
            if rec == "No":
                context = "For context, a board member recommended to keep an existing reporting policy."
            if rec == "No opinion":
                context = "For context, a board member has no opinion regarding a proposed change to an existing reporting policy."

            # Prompt
            messages = [{"role": "user", "content": context + "\
                    The board member explained his recommendation with the following statement:\n\"" + statement + "\"\n\
                    I want you to classify this statement into the following categories:\n\
                    A. Reporting Motives. The board member is concerned about undesirable impacts on the current stock price, in terms of abnormal positive or negative stock returns, or increase in volatility. They have a preference for reporting policies that manage the current stock price, even if the policy involves bias or untruthful reporting.\n\
                    B. Legal Liability. The board member considers the legal risks, such as risks from investor lawsuits or the consequences of regulatory investigations by agencies like the SEC. This includes risks created by non-compliance, regardless of whether the policy is ethical or affects investors.\n\
                    C. Long-term focus. The board member believes that policies should be chosen with a focus on the long-term, even if they may cause costs or disruptions in the short-term. In particular, short-term challenges due to unexpected changes will dissipate over time as investors adapt, consistent with feedback and learning.\n\
                    D. Social Norms. The board member justifies the policy based on generally-accepted social norms and industry standards. A policy may be considered permissible if it aligns with widespread practices, even if it is untruthful or may hurt investors.\n\
                    E. Investor Harm. A board member views the policy as misleading or confusing if it is not adequately anticipated or understood by investors. This concern arises particularly when changes in policy or reporting lead to misinterpretation, potentially resulting in decisions that negatively impact investors’ outcomes.\n\
                    F. Truthfulness. The board member believes that the firm should prioritize truthfulness toward their investors, reporting earnings as-is even in cases where investors may come to an incorrect inference if they expect a bias. Bias is inherently unethical.\n\
                    Only select the categories the board member agrees with.\
                    If the category refers to an argument raised by the CFO or the CEO but the board member does not agree with it, do not select it.\
                    You can categorize the statement into multiple categories or none if appropriate.\n\
                    Additionally, please identify which category seems to be the most important for the board member in its decision.\n\
                    Provide your response as the following json object:\
                    {\"A\": Yes/No answer,\
                    \"B\": Yes/No answer,\
                    \"C\": Yes/No answer,\
                    \"D\": Yes/No answer,\
                    \"E\": Yes/No answer,\
                    \"F\": Yes/No answer,\
                    \"most important category\": Letter answer (A to F) identifying the most important category or None if appropriate\
                    }"}]

            # Response
            response = client.chat.completions.create(
                model="gpt-4o",
                messages=messages,
                stream=False,
                max_tokens=200,
                response_format = {"type": "json_object"})
            # Extract answer
            a = json.loads(response.choices[0].message.content)
            a["id"] = r.id
            answers += [a]

    dfta = pd.DataFrame(answers)
    dfta.to_csv("surveys/textual_analyses/textual_analysis_humans.csv", index=False)

def run_textual_analysis_recommendation(model, survey, temperature=None):
    # Uses gpt-4o for the textual analysis
    url = "https://api.openai.com/v1"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("OPENAI_API_KEY")
    if api_key is None:
        try:
            with open("OPENAI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No OpenAI API key found.")

    client = OpenAI(
        base_url=url,
        api_key=api_key
    )

    survey_name = survey.replace(".json", "").replace("survey_", "")

    df = tabulate_surveys(model, survey, temperature=temperature)
    answers = []
    for id, rec, statement in zip(df.id, df.e3, df.e4):
        context = "For context, a board member recommended to change an existing reporting policy."
        if rec == "No":
            context = "For context, a board member recommended to keep an existing reporting policy."
        if rec == "No opinion":
            context = "For context, a board member has no opinion regarding a proposed change to an existing reporting policy."

        print("Textual Analysis for " + model + " / " + survey + " ID: " + id + " Temperature: " + str(temperature), end="\r")
        # Prompt
        messages = [{"role": "user", "content": context + "\
                The board member explained his recommendation with the following statement:\n\"" + statement + "\"\n\
                I want you to classify this statement into the following categories:\n\
                A. Reporting Motives. The board member is concerned about undesirable impacts on the current stock price, in terms of abnormal positive or negative stock returns, or increase in volatility. They have a preference for reporting policies that manage the current stock price, even if the policy involves bias or untruthful reporting.\n\
                B. Legal Liability. The board member considers the legal risks, such as risks from investor lawsuits or the consequences of regulatory investigations by agencies like the SEC. This includes risks created by non-compliance, regardless of whether the policy is ethical or affects investors.\n\
                C. Long-term focus. The board member believes that policies should be chosen with a focus on the long-term, even if they may cause costs or disruptions in the short-term. In particular, short-term challenges due to unexpected changes will dissipate over time as investors adapt, consistent with feedback and learning.\n\
                D. Social Norms. The board member justifies the policy based on generally-accepted social norms and industry standards. A policy may be considered permissible if it aligns with widespread practices, even if it is untruthful or may hurt investors.\n\
                E. Investor Harm. A board member views the policy as misleading or confusing if it is not adequately anticipated or understood by investors. This concern arises particularly when changes in policy or reporting lead to misinterpretation, potentially resulting in decisions that negatively impact investors’ outcomes.\n\
                F. Truthfulness. The board member believes that the firm should prioritize truthfulness toward their investors, reporting earnings as-is even in cases where investors may come to an incorrect inference if they expect a bias. Bias is inherently unethical.\n\
                Only select the categories the board member agrees with.\
                If the category refers to an argument raised by the CFO or the CEO but the board member does not agree with it, do not select it.\
                You can categorize the statement into multiple categories or none if appropriate.\n\
                Additionally, please identify which category seems to be the most important for the board member in its decision.\n\
                Provide your response as the following json object:\
                {\"A\": Yes/No answer,\
                \"B\": Yes/No answer,\
                \"C\": Yes/No answer,\
                \"D\": Yes/No answer,\
                \"E\": Yes/No answer,\
                \"F\": Yes/No answer,\
                \"most important category\": Letter answer (A to F) identifying the most important category or None if appropriate\
                }"}]

        # Response
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=messages,
            stream=False,
            max_tokens=200,
            response_format = {"type": "json_object"})
        # Extract answer
        a = json.loads(response.choices[0].message.content)
        a["model"] = model
        a["survey"] = survey_name
        a["id"] = id
        answers += [a]

    dfta = pd.DataFrame(answers)
    filename = "surveys/textual_analyses/textual_analysis_" + model + "_" + survey_name + ".csv"
    if temperature is not None:
        filename = "surveys/textual_analyses/textual_analysis_" + model + "_" + survey_name + "_temp_" + str(temperature) + ".csv"
    dfta.to_csv(filename, index=False)

def run_all_textual_analyses_recommendation():

    models = [
            # Local Models
            # "llama3.2:1b",
            # "llama3.2:3b",
            # "gemma2:2b",
            "mistral:7b",
            # "phi3.5:3.8b",
            "llama3.1:8b",
            # "gemma2:9b",
            # "gemma2:27b",
            # "llama3.1:70b",
            # Online Models
            "gpt-4o",
            # "gpt-4o-mini",
            "gemini-1.5-flash-002",
            # "gemini-1.5-flash-8b"
            ]

    for m in models:
        for s in ["survey_overreporting.json", "survey_underreporting.json"]:
            run_textual_analysis_recommendation(m, s)

    # Models with higher temperature
    models_temp = [
            # "mistral:7b",
            # "llama3.1:8b",
            # "gpt-4o",
            # "gemini-1.5-flash-002",
            ]

    for m in models_temp:
        for s in ["survey_overreporting.json", "survey_underreporting.json"]:
            run_textual_analysis_recommendation(m, s, temperature=1.4)

# 2. For definition of ethical reporting policy

def run_human_textual_analysis_ethical(file="../human_survey/data.csv"):
    # Uses gpt-4o for the textual analysis
    url = "https://api.openai.com/v1"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("OPENAI_API_KEY")
    if api_key is None:
        try:
            with open("OPENAI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No OpenAI API key found.")

    client = OpenAI(
        base_url=url,
        api_key=api_key
    )

    df = pd.read_csv(file)
    df = df[["id", "Q15", "Q15.1"]][8:174]
    answers = []
    for i in range(0, len(df)):
        print("Textual Analyses (Ethical) for Humans (" + str(i+1) + " / " + str(len(df)) + ")", end="\r")
        r = df.iloc[i, :]
        statement = r["Q15"]
        if isinstance(statement, float):
            statement = r["Q15.1"]
        if isinstance(statement, str):
            # Prompt
            messages = [{"role": "user", "content": "\
                    For context, a board member explained the characteristics of a reporting policy that would be viewed as ethical with the following statement:\n\"" + statement + "\"\n\
                    I want you to classify this statement into the following categories:\n\
                    A. Reporting Motives. The board member is concerned about undesirable impacts on the current stock price, in terms of abnormal positive or negative stock returns, or increase in volatility. They have a preference for reporting policies that manage the current stock price, even if the policy involves bias or untruthful reporting.\n\
                    B. Legal Liability. The board member considers the legal risks, such as risks from investor lawsuits or the consequences of regulatory investigations by agencies like the SEC. This includes risks created by non-compliance, regardless of whether the policy is ethical or affects investors.\n\
                    C. Long-term focus. The board member believes that policies should be chosen with a focus on the long-term, even if they may cause costs or disruptions in the short-term. In particular, short-term challenges due to unexpected changes will dissipate over time as investors adapt, consistent with feedback and learning.\n\
                    D. Social Norms. The board member justifies the policy based on generally-accepted social norms and industry standards. A policy may be considered permissible if it aligns with widespread practices, even if it is untruthful or may hurt investors.\n\
                    E. Investor Harm. A board member views the policy as misleading or confusing if it is not adequately anticipated or understood by investors. This concern arises particularly when changes in policy or reporting lead to misinterpretation, potentially resulting in decisions that negatively impact investors’ outcomes.\n\
                    F. Truthfulness. The board member believes that the firm should prioritize truthfulness toward their investors, reporting earnings as-is even in cases where investors may come to an incorrect inference if they expect a bias. Bias is inherently unethical.\n\
                    Only select the categories the board member mentions.\
                    You can categorize the statement into multiple categories or none if appropriate.\n\
                    Additionally, please identify which category seems to be the most important for the board member.\n\
                    Provide your response as the following json object:\
                    {\"A\": Yes/No answer,\
                    \"B\": Yes/No answer,\
                    \"C\": Yes/No answer,\
                    \"D\": Yes/No answer,\
                    \"E\": Yes/No answer,\
                    \"F\": Yes/No answer,\
                    \"most important category\": Letter answer (A to F) identifying the most important category or None if appropriate\
                    }"}]

            # Response
            response = client.chat.completions.create(
                model="gpt-4o",
                messages=messages,
                stream=False,
                max_tokens=200,
                response_format = {"type": "json_object"})
            # Extract answer
            a = json.loads(response.choices[0].message.content)
            a["id"] = r.id
            answers += [a]

    dfta = pd.DataFrame(answers)
    dfta.to_csv("surveys/textual_analyses/textual_analysis_ethical_humans.csv", index=False)

def run_textual_analysis_ethical(model, survey, temperature=None):
    # Uses gpt-4o for the textual analysis
    url = "https://api.openai.com/v1"
    # Check if environment variable OPENAI_API_KEY exists
    api_key = os.environ.get("OPENAI_API_KEY")
    if api_key is None:
        try:
            with open("OPENAI_API_KEY") as f:
                api_key = f.readline().strip('\n')
        except Exception:
            raise Exception("No OpenAI API key found.")

    client = OpenAI(
        base_url=url,
        api_key=api_key
    )

    survey_name = survey.replace(".json", "").replace("survey_", "")

    df = tabulate_surveys(model, survey, temperature=temperature)
    answers = []
    for id, statement in zip(df.id, df.e5):
        print("Textual Analysis (Ethical) for " + model + " / " + survey + " ID: " + id + " Temperature: " + str(temperature), end="\r")
        # Prompt
        messages = [{"role": "user", "content": "\
                For context, a board member explained the characteristics of a reporting policy that would be viewed as ethical with the following statement:\n\"" + statement + "\"\n\
                I want you to classify this statement into the following categories:\n\
                A. Reporting Motives. The board member is concerned about undesirable impacts on the current stock price, in terms of abnormal positive or negative stock returns, or increase in volatility. They have a preference for reporting policies that manage the current stock price, even if the policy involves bias or untruthful reporting.\n\
                B. Legal Liability. The board member considers the legal risks, such as risks from investor lawsuits or the consequences of regulatory investigations by agencies like the SEC. This includes risks created by non-compliance, regardless of whether the policy is ethical or affects investors.\n\
                C. Long-term focus. The board member believes that policies should be chosen with a focus on the long-term, even if they may cause costs or disruptions in the short-term. In particular, short-term challenges due to unexpected changes will dissipate over time as investors adapt, consistent with feedback and learning.\n\
                D. Social Norms. The board member justifies the policy based on generally-accepted social norms and industry standards. A policy may be considered permissible if it aligns with widespread practices, even if it is untruthful or may hurt investors.\n\
                E. Investor Harm. A board member views the policy as misleading or confusing if it is not adequately anticipated or understood by investors. This concern arises particularly when changes in policy or reporting lead to misinterpretation, potentially resulting in decisions that negatively impact investors’ outcomes.\n\
                F. Truthfulness. The board member believes that the firm should prioritize truthfulness toward their investors, reporting earnings as-is even in cases where investors may come to an incorrect inference if they expect a bias. Bias is inherently unethical.\n\
                Only select the categories the board member mentions.\
                You can categorize the statement into multiple categories or none if appropriate.\n\
                Additionally, please identify which category seems to be the most important for the board member.\n\
                Provide your response as the following json object:\
                {\"A\": Yes/No answer,\
                \"B\": Yes/No answer,\
                \"C\": Yes/No answer,\
                \"D\": Yes/No answer,\
                \"E\": Yes/No answer,\
                \"F\": Yes/No answer,\
                \"most important category\": Letter answer (A to F) identifying the most important category or None if appropriate\
                }"}]

        # Response
        response = client.chat.completions.create(
            model="gpt-4o",
            messages=messages,
            stream=False,
            max_tokens=200,
            response_format = {"type": "json_object"})
        # Extract answer
        a = json.loads(response.choices[0].message.content)
        a["model"] = model
        a["survey"] = survey_name
        a["id"] = id
        answers += [a]

    dfta = pd.DataFrame(answers)
    filename = "surveys/textual_analyses/textual_analysis_ethical_" + model + "_" + survey_name + ".csv"
    if temperature is not None:
        filename = "surveys/textual_analyses/textual_analysis_ethical_" + model + "_" + survey_name + "_temp_" + str(temperature) + ".csv"
    dfta.to_csv(filename, index=False)

def run_all_textual_analyses_ethical():

    models = [
            # Local Models
            # "llama3.2:1b",
            "llama3.2:3b",
            "gemma2:2b",
            # "mistral:7b",
            # "phi3.5:3.8b",
            # "llama3.1:8b",
            "gemma2:9b",
            "gemma2:27b",
            "llama3.1:70b",
            # Online Models
            # "gpt-4o",
            "gpt-4o-mini",
            # "gemini-1.5-flash-002",
            "gemini-1.5-flash-8b"
            ]

    for m in models:
        for s in ["survey_overreporting.json", "survey_underreporting.json"]:
            run_textual_analysis_ethical(m, s)

    # Models with higher temperature
    models_temp = [
            # "mistral:7b",
            # "llama3.1:8b",
            # "gpt-4o",
            # "gemini-1.5-flash-002",
            ]

    for m in models_temp:
        for s in ["survey_overreporting.json", "survey_underreporting.json"]:
            run_textual_analysis_ethical(m, s, temperature=1.4)


# Functions to extract the data

def tabulate_surveys(model, survey, temperature=None):
    model = model.replace("/", "_")
    survey_name = survey.replace(".json", "")
    all_files = os.listdir("surveys/")
    r = re.compile('^answers_'+survey_name+'_'+model+'_id_[0-9]+_seed_([0-9]+|None).json$')
    if temperature is not None:
        r = re.compile('^answers_'+survey_name+'_'+model+'_id_[0-9]+_seed_([0-9]+|None)_temp_'+str(temperature)+'.json$')
    files_answers = [f for f in all_files if r.match(f)]
    answers_list = []
    for file in files_answers:
        # Extract id
        i = re.search('_id_([0-9]*)_', file).groups()[0]
        with open("surveys/"+file) as f:
            a = json.load(f)
            a["id"] = i
            a["temperature"] = temperature
            answers_list += [a]
    return pd.DataFrame(answers_list)

def tabulate_all_surveys():
    dfs = {}
    # Ollama
    for m in models_ollama:
        dfs[m] = tabulate_surveys(m, "survey_over.json")
    return dfs

# Function to create the final datasets

def data_human_survey(file="../human_survey/data.csv"):
    df = pd.read_csv(file, dtype={'Q78': str, 'Q79': str})
    # Note: row 2 -> description, rows 3 and 4 -> tests (do not use)
    df = df[8:174]
    ########################
    # Add Textual Analysis #
    ########################
    # 1. Recommendation
    df.id = df.id.astype(int)
    dfta = pd.read_csv("surveys/textual_analyses/textual_analysis_humans.csv")
    for q in ["A", "B", "C", "D", "E", "F"]:
        dfta[q] = dfta[q].map({"No": 0, "Yes": 1})
    dfta.rename(columns={
        "A": "explanation_reporting",
        "B": "explanation_legal",
        "C": "explanation_stlt",
        "D": "explanation_norm",
        "E": "explanation_harm",
        "F": "explanation_kant",
        "most important category": "explanation_main"
        }, inplace=True)

    df = df.merge(dfta, on="id", how="left")
    # 2. Ethical
    dfta = pd.read_csv("surveys/textual_analyses/textual_analysis_ethical_humans.csv")
    for q in ["A", "B", "C", "D", "E", "F"]:
        dfta[q] = dfta[q].map({"No": 0, "Yes": 1})
    dfta.rename(columns={
        "A": "ethical_reporting",
        "B": "ethical_legal",
        "C": "ethical_stlt",
        "D": "ethical_norm",
        "E": "ethical_harm",
        "F": "ethical_kant",
        "most important category": "ethical_main"
        }, inplace=True)

    df = df.merge(dfta, on="id", how="left")

    # Over-reporting
    dfo = df.dropna(subset=["Q104"]).reset_index()
    # Under-reporting
    dfu = df.dropna(subset=["Q120"]).reset_index()

    ##################
    # Over-reporting #
    ##################
    # id
    # df = pd.DataFrame({"id": range(1, len(dfo)+1), "type": "human", "survey":"overreporting"})
    df = dfo[["id",
              "explanation_reporting", "explanation_legal",
              "explanation_stlt", "explanation_norm",
              "explanation_harm", "explanation_kant",
              "explanation_main",
              "ethical_reporting", "ethical_legal",
              "ethical_stlt", "ethical_norm",
              "ethical_harm", "ethical_kant",
              "ethical_main"
              ]]
    df["type"] = "human"
    df["survey"] = "overreporting"

    d = {
        "Q11_1": "current_policy_ethic",
        "Q11_2": "new_policy_ethic"
    }
    for k in d:
        v = dfo[k]
        v = v.replace("1 (least ethical)", 1)
        v = v.replace("5 (most ethical)", 5)
        v = v.astype(int)
        df[d[k]] = v

    d = {
        "Q12_1": "current_policy_lying",
        "Q12_2": "new_policy_lying",
        "Q12_3": "current_policy_deceptive",
        "Q12_4": "new_policy_deceptive",
        "Q12_5": "current_policy_dishonest",
        "Q12_6": "new_policy_dishonest",
        "Q12_7": "current_policy_untruthful",
        "Q12_8": "new_policy_untruthful",
    }
    for k in d:
        v = dfo[k]
        v = v.map({"Disagree strongly": 1, "Disagree": 2, "Neither agree or disagree": 3, "Agree": 4, "Agree strongly": 5})
        df[d[k]] = v

    # recommend_new_policy
    v = dfo.Q13
    # v = v.map({"No": 0, "Yes": 1, "No opinion": 2})
    df["recommend_new_policy"] = v

    # board_vote
    v = dfo.Q17
    v = v.map({"Against (retain current policy)": "Against", "In Favor (adopt new policy)": "In Favor", "Follow the consensus": "Follow Consensus", "Abstain": "Abstain"})
    df["board_vote"] = v

    # Indentifying information
    df["age"] = dfo.Q1
    df["race"] = dfo.Q5
    df["race_other"] = dfo.Q5_7_TEXT
    df["gender"] = dfo.Q37
    df["gender_other"] = dfo.Q37_4_TEXT
    df["study"] = dfo.Q38
    df["gpa"] = dfo.Q39
    df["risk_tolerance"] = dfo.Q40_1
    df["political_affiliation"] = dfo.Q42
    df["political_affiliation_other"] = dfo.Q42_4_TEXT
    df["cheating_answer"] = dfo.Q83             # Intestellar object
    df["score_self_reported"] = dfo.Q80
    df["score_true"] = dfo.SC0
    df["score_20"] = dfo.Q79
    df["score_20_truthful"] = dfo.Q78

    dfo = df

    ##################
    # Under-reporting #
    ##################
    # id
    # df = pd.DataFrame({"id": range(1, len(dfu)+1), "type": "human", "survey":"underreporting"})
    df = dfu[["id",
              "explanation_reporting", "explanation_legal",
              "explanation_stlt", "explanation_norm",
              "explanation_harm", "explanation_kant",
              "explanation_main",
              "ethical_reporting", "ethical_legal",
              "ethical_stlt", "ethical_norm",
              "ethical_harm", "ethical_kant",
              "ethical_main"
              ]]
    df["type"] = "human"
    df["survey"] = "underreporting"

    d = {
        "Q11_1.1": "current_policy_ethic",
        "Q11_2.1": "new_policy_ethic"
    }
    for k in d:
        v = dfu[k]
        v = v.replace("1 (least ethical)", 1)
        v = v.replace("5 (most ethical)", 5)
        v = v.astype(int)
        df[d[k]] = v

    d = {
        "Q12_1.1": "current_policy_lying",
        "Q12_2.1": "new_policy_lying",
        "Q12_3.1": "current_policy_deceptive",
        "Q12_4.1": "new_policy_deceptive",
        "Q12_5.1": "current_policy_dishonest",
        "Q12_6.1": "new_policy_dishonest",
        "Q12_7.1": "current_policy_untruthful",
        "Q12_8.1": "new_policy_untruthful",
    }
    for k in d:
        v = dfu[k]
        v = v.map({"Disagree strongly": 1, "Disagree": 2, "Neither agree or disagree": 3, "Agree": 4, "Agree strongly": 5})
        df[d[k]] = v

    # recommend_new_policy
    v = dfu["Q13.1"]
    # v = v.map({"No": 0, "Yes": 1, "No opinion": 2})
    df["recommend_new_policy"] = v

    # board_vote
    v = dfu["Q17.1"]
    # v = v.map({"Against (retain current policy)": 0, "In Favor (adopt new policy)": 1, "Follow the consensus": 2, "Abstain": 3})
    v = v.map({"Against (retain current policy)": "Against", "In Favor (adopt new policy)": "In Favor", "Follow the consensus": "Follow Consensus", "Abstain": "Abstain"})
    df["board_vote"] = v

    # Indentifying information
    df["age"] = dfu.Q1
    df["race"] = dfu.Q5
    df["race_other"] = dfu.Q5_7_TEXT
    df["gender"] = dfu.Q37
    df["gender_other"] = dfu.Q37_4_TEXT
    df["study"] = dfu.Q38
    df["gpa"] = dfu.Q39
    df["risk_tolerance"] = dfu.Q40_1
    df["political_affiliation"] = dfu.Q42
    df["political_affiliation_other"] = dfu.Q42_4_TEXT
    df["cheating_answer"] = dfu.Q83         # Intestellar object
    df["score_self_reported"] = dfu.Q80
    df["score_true"] = dfu.SC0
    df["score_20"] = dfu.Q79
    df["score_20_truthful"] = dfu.Q78

    dfu = df

    df = pd.concat([dfo, dfu], axis=0)

    #################
    # Data cleaning #
    #################
    # Gender
    df["male"] = 0
    df.loc[df.gender=="Male", "male"] = 1
    # Political affiliation (-1: democrat, 1: republican)
    df.political_affiliation = df.political_affiliation.map({"Democrat": -1, "Independent": 0, "Other": 0, "Republican": 1})
    # Field of study
    df.study = df.study.str.lower().str.replace(' ', '')
    business_econ_nolist = [
        'olin(undecided)',
        'communicationdesign',
        'english',
        'architecture',
        'politicalstudy',
        'electricalengineering',
        'psychological&brainsciences',
        'undecide',
        'psychology',
        'undecided',
        'internationalaffairs',
        'politicalscience',
        'pnp',
    ]
    df["business_econ"] = 1
    df.loc[df.study.isin(business_econ_nolist), "business_econ"] = 0
    # Under-represented minorities
    df["under_represented"] = 1
    under_represented_nolist = [
        'White',
        'Asian',
        'White,Asian',
        'White,Other',
        'Other'
    ]
    df.loc[df.race.isin(under_represented_nolist), "under_represented"] = 0
    # GPA
    df.gpa = pd.to_numeric(df.gpa, errors='coerce')
    # Cheating (if responded to interstellar object correctly)
    df["cheating"] = 0
    cheating_list = [
            "oumuamua", "Oumuamua", "Oumuamua",
            "omuamua", "Oumuamua Overview"
            ]
    df.loc[df.cheating_answer.isin(cheating_list), "cheating"] = 1
    # Lying = score self-reported - true score
    df["misreporting"] = df.score_self_reported.astype(float) - df.score_true.astype(float)
    # Trust = score 20% truthful - score 20%
    df.score_20 = df.score_20.str.replace("/10", "")
    df.score_20 = df.score_20.str.replace(" out of 10", "")
    df.score_20 = df.score_20.str.replace("-", "")
    df.score_20 = df.score_20.str.replace("yes", "")
    df.score_20 = df.score_20.str.replace("98%", "9.8")
    df.score_20 = df.score_20.str.replace("65", "6.5")
    df.score_20 = df.score_20.str.replace("80", "8")
    df.score_20 = df.score_20.str.replace("55", "5.5")
    df.score_20 = df.score_20.str.replace("90%", "9")
    df.score_20 = df.score_20.str.replace("45", "4.5")
    df.score_20 = df.score_20.str.replace("20", "2")
    df.score_20 = df.score_20.str.replace("I am a singular frame of reference. 8. ", "8")
    df.score_20 = df.score_20.str.replace(" ", "")
    df.score_20 = pd.to_numeric(df.score_20, errors='coerce')

    df.score_20_truthful = df.score_20_truthful.str.replace("/10", "")
    df.score_20_truthful = df.score_20_truthful.str.replace(" out of 10", "")
    df.score_20_truthful = df.score_20_truthful.str.replace(" out of ten", "")
    df.score_20_truthful = df.score_20_truthful.str.replace("20", "2")
    df.score_20_truthful = df.score_20_truthful.str.replace("40", "4")
    df.score_20_truthful = df.score_20_truthful.str.replace("50", "5")
    df.score_20_truthful = df.score_20_truthful.str.replace("60%", "6")
    df.score_20_truthful = df.score_20_truthful.str.replace("70", "7")
    df.score_20_truthful = df.score_20_truthful.str.replace("75%", "7.5")
    df.score_20_truthful = df.score_20_truthful.str.replace("80%", "8")
    df.score_20_truthful = df.score_20_truthful.str.replace("80", "8")
    df.score_20_truthful = df.score_20_truthful.str.replace("I know way more than 6 of these, so 8 ", "8")
    df.score_20_truthful = pd.to_numeric(df.score_20_truthful, errors='coerce')

    df["trust"] = df.score_20_truthful - df.score_20

    return df

def data_llm_survey(model, temperature=None):
    dfs_all = []

    for side in ["overreporting", "underreporting"]:

        dfs = tabulate_surveys(model, "survey_"+side+".json", temperature=temperature)

        ####################
        # Textual Analysis #
        ####################
        cols = ["id"]
        # 1. Recommendation
        filename = "surveys/textual_analyses/textual_analysis_" + model + "_" + side + ".csv"
        if temperature is not None:
            filename = "surveys/textual_analyses/textual_analysis_" + model + "_" + side + "_temp_" + str(temperature) + ".csv"
        if os.path.isfile(filename):
            dfta = pd.read_csv(filename)
            dfta.drop(columns=["model", "survey"], inplace=True)
            dfs.id = dfs.id.astype(int)
            for q in ["A", "B", "C", "D", "E", "F"]:
                dfta[q] = dfta[q].map({"No": 0, "Yes": 1})
            dfta.rename(columns={
                "A": "explanation_reporting",
                "B": "explanation_legal",
                "C": "explanation_stlt",
                "D": "explanation_norm",
                "E": "explanation_harm",
                "F": "explanation_kant",
                "most important category": "explanation_main"
                }, inplace=True)

            dfs = dfs.merge(dfta, on="id", how="left")
            cols += ["explanation_reporting", "explanation_legal",
                     "explanation_stlt", "explanation_norm",
                     "explanation_harm", "explanation_kant",
                     "explanation_main"]


        # 2. Ethical
        filename = "surveys/textual_analyses/textual_analysis_ethical_" + model + "_" + side + ".csv"
        if temperature is not None:
            filename = "surveys/textual_analyses/textual_analysis_ethical_" + model + "_" + side + "_temp_" + str(temperature) + ".csv"
        if os.path.isfile(filename):
            dfta = pd.read_csv(filename)
            for q in ["A", "B", "C", "D", "E", "F"]:
                dfta[q] = dfta[q].map({"No": 0, "Yes": 1})
            dfta.rename(columns={
                "A": "ethical_reporting",
                "B": "ethical_legal",
                "C": "ethical_stlt",
                "D": "ethical_norm",
                "E": "ethical_harm",
                "F": "ethical_kant",
                "most important category": "ethical_main"
                }, inplace=True)

            dfs = dfs.merge(dfta, on="id", how="left")
            cols += ["ethical_reporting", "ethical_legal",
                     "ethical_stlt", "ethical_norm",
                     "ethical_harm", "ethical_kant",
                     "ethical_main"]

        df = dfs[cols]
        df["type"] = "llm"
        df["model"] = model
        df["temperature"] = temperature
        df["survey"] = side

        df["current_policy_ethic"] = [k['current_policy'] for k in dfs.e1]
        df["new_policy_ethic"] = [k['new_policy'] for k in dfs.e1]

        df["current_policy_lying"] = [k['a'] for k in dfs.e2]
        df["new_policy_lying"] = [k['b'] for k in dfs.e2]
        df["current_policy_deceptive"] = [k['c'] for k in dfs.e2]
        df["new_policy_deceptive"] = [k['d'] for k in dfs.e2]
        df["current_policy_dishonest"] = [k['e'] for k in dfs.e2]
        df["new_policy_dishonest"] = [k['f'] for k in dfs.e2]
        df["current_policy_untruthful"] = [k['g'] for k in dfs.e2]
        df["new_policy_untruthful"] = [k['h'] for k in dfs.e2]

        # df["recommend_new_policy"] = dfs.e3.map({"No": 0, "Yes": 1, "No opinion": 2})
        df["recommend_new_policy"] = dfs.e3
        df["board_vote"] = dfs.e7.map({"Against": "Against", "In Favor": "In Favor", "Follow": "Follow Consensus", "Abstain": "Abstain"})

        dfs_all += [df]

    df = pd.concat(dfs_all, axis=0)
    return df

def create_data_survey():
    # Combine all survey data (human and all LLMs)
    df = data_human_survey()

    models = [
            "llama3.2:3b",
            "llama3.1:8b",
            "llama3.1:70b",
            "gemma2:2b",
            "gemma2:9b",
            "gemma2:27b",
            "mistral:7b",
            "gpt-4o",
            "gpt-4o-mini",
            "gemini-1.5-flash-002",
            "gemini-1.5-flash-8b"
            ]

    for m in models:
        print(m)
        dfl = data_llm_survey(m)
        df = pd.concat([df, dfl])

    # Add models with higher temperature
    models_temp = [
            "llama3.1:8b",
            "mistral:7b",
            "gpt-4o",
            "gemini-1.5-flash-002",
            ]

    for m in models_temp:
        print(m)
        dfl = data_llm_survey(m, temperature=1.4)
        df = pd.concat([df, dfl])

    df.to_csv("../data_survey.csv", index=False)



