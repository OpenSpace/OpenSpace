import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Input from '../../common/Input/Input/Input';
import Label from '../../common/Label/Label';
import Row from '../../common/Row/Row';
import styles from './OptionSelect.scss';

const OptionSelect = (props) => {
  const { options, onChange, label, subLabel } = props;
  return (
    <div className={styles.optionSelect}>
      <Label size='medium'>{label}:</Label>
        <Row className="hej">
          { Object.keys(options).map((key, index) => (
              <Input 
                key={key}
                label={key}
                placeholder={key}
                value={options[key]}
                onChange={onChange}
              />
          ))}
        </Row>
    </div>
  );
};

OptionSelect.propTypes = {
  options: PropTypes.object.isRequired,
  label: PropTypes.string,
  onChange: PropTypes.func,
}

OptionSelect.defaultProps = {
  options: {},
  label:'Loading',
  onChange: () => {},
}

export default OptionSelect;